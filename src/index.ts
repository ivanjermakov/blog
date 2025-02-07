import { spawn } from 'child_process'
import { existsSync, mkdirSync } from 'fs'
import { copyFile, cp, mkdir, readFile, readdir, writeFile } from 'fs/promises'
import { PostMetainfo, postsMeta } from './posts-meta'

const tags = [
    ...new Set(
        Object.values(postsMeta)
            .filter(m => m.draft !== true)
            .flatMap(m => m.tags)
    )
]

const templateDirPath = 'template'
const templateFiles = await readdir(templateDirPath)
const templates = Object.fromEntries(
    (await Promise.all(templateFiles.map(tf => readFile(`${templateDirPath}/${tf}`)))).map((t, i) => [
        templateFiles[i],
        t.toString()
    ])
)
const outDirPath = 'dist'
await mkdir(outDirPath, { recursive: true })

const cssDirPath = 'css'
await cp(cssDirPath, `${outDirPath}/${cssDirPath}`, { recursive: true })

const indexPage = makeIndexPage()
const indexPagePath = `${outDirPath}/index.html`
console.info(indexPagePath)
writeFile(indexPagePath, indexPage)

const postDirPath = 'post'
const outPostDirPath = `${outDirPath}/post`
await mkdir(outPostDirPath, { recursive: true })
await Promise.all(
    Object.entries(postsMeta).map(async ([pName, pm]) => {
        const pathDir = `${postDirPath}/${pName}`
        const pathMd = `${postDirPath}/${pName}.md`
        const pathHtml = `${postDirPath}/${pName}.html`
        if (existsSync(pathDir)) {
            const postPath = `${outPostDirPath}/${pName}`
            mkdirSync(postPath, { recursive: true })
            await Promise.all(
                (pm.include ?? []).map(i => {
                    console.info(`${pathDir}/${i} -> ${postPath}/${i}`)
                    copyFile(`${pathDir}/${i}`, `${postPath}/${i}`)
                })
            )
            const content = (await readFile(`${pathDir}/index.html`)).toString()
            const postPage = await makePostPage(pName, pm, content)
            const postHtmlPath = `${outPostDirPath}/${pName}.html`
            console.info(postHtmlPath)
            await writeFile(postHtmlPath, postPage)
        } else if (existsSync(pathMd)) {
            const postMd = (await readFile(pathMd)).toString()
            const postPage = await makePostPage(pName, pm, await mdToHtml(postMd))
            const postPath = `${outPostDirPath}/${pName}.html`
            await writeFile(postPath, postPage)
        } else if (existsSync(pathHtml)) {
            const content = (await readFile(pathHtml)).toString()
            const postPage = await makePostPage(pName, pm, content)
            const postPath = `${outPostDirPath}/${pName}.html`
            console.info(postPath)
            await writeFile(postPath, postPage)
        }
    })
)

const outTagDirPath = `${outDirPath}/tag`
await mkdir(outTagDirPath, { recursive: true })
tags.map(tag => {
    const tagPage = makeIndexPage(tag)
    const tagPagePath = `${outTagDirPath}/${tag}.html`
    console.info(tagPagePath)
    return writeFile(tagPagePath, tagPage)
})

const notFoundPage = replaceVariables(templates['index.html'], {
    title: 'ivnj - 404',
    description: 'not found',
    body: templates['404.html']
})
const notFoundPagePath = `${outDirPath}/404.html`
console.info(notFoundPagePath)
writeFile(notFoundPagePath, notFoundPage)

const aboutPage = replaceVariables(templates['index.html'], {
    title: 'ivnj - about',
    description: 'about',
    body: templates['about.html']
})
const aboutPagePath = `${outDirPath}/about.html`
console.info(aboutPagePath)
writeFile(aboutPagePath, aboutPage)

const tagsPage = makeTagsPage()
const tagsPagePath = `${outDirPath}/tags.html`
console.info(tagsPagePath)
writeFile(tagsPagePath, tagsPage)

function makeIndexPage(forTag?: string): string {
    const postItemsFragment = Object.entries(postsMeta)
        .filter(([, pm]) => pm.draft !== true)
        .filter(([, pm]) => !forTag || pm.tags.includes(forTag))
        .map(([pName, pm]) => {
            const tagsFragment = pm.tags.map(t => templates['tag.html'].replaceAll('$title$', t)).join('')

            return replaceVariables(templates['post-item.html'], {
                url: `/post/${pName}.html`,
                title: pm.title,
                tags: tagsFragment,
                date: pm.date,
                description: pm.description
            })
        })
        .join('\n')
    return replaceVariables(templates['index.html'], {
        title: 'ivnj blog',
        description: 'ivnj blog',
        body: postItemsFragment
    })
}

async function makePostPage(name: string, metainfo: PostMetainfo, content: string): Promise<string> {
    const tagsFragment = metainfo.tags.map(t => replaceVariables(templates['tag.html'], { title: t })).join('')
    const postFragment = replaceVariables(templates['post.html'], {
        title: metainfo.title,
        date: metainfo.date,
        tags: tagsFragment,
        body: content
    })
    return replaceVariables(templates['index.html'], {
        title: metainfo.title,
        description: metainfo.description,
        body: postFragment
    })
}

function makeTagsPage(): string {
    const tagFragments = tags.map(t => replaceVariables(templates['tag.html'], { title: t }))
    const tagsFragment = replaceVariables(templates['tags.html'], { tag: tagFragments.join('') })
    return replaceVariables(templates['index.html'], {
        title: 'ivnj blog',
        description: 'ivnj blog',
        body: tagsFragment
    })
}

function replaceVariables(template: string, vars: Record<string, string>): string {
    return Object.entries(vars).reduce((t, [vk, v]) => t.replaceAll(`$${vk}$`, v), template)
}

function mdToHtml(md: string): Promise<string> {
    return new Promise((resolve, reject) => {
        const extensions = [
            'smart',
            'pipe_tables',
            'raw_html',
            'raw_attribute',
            'native_divs',
            'auto_identifiers',
            'gfm_auto_identifiers',
            'autolink_bare_uris',
            'strikeout',
            'task_lists',
            'emoji',
            'fenced_code_blocks',
            'backtick_code_blocks',
            'tex_math_dollars',
            'tex_math_single_backslash',
            'tex_math_double_backslash',
            'implicit_header_references',
            'implicit_figures',
            'abbreviations',
            'fenced_divs'
        ]
        const opts = ['--wrap=preserve', '--mathjax']
        const process = spawn('pandoc', [...opts, `--from=markdown+${extensions.join('+')}`])

        let stdoutData = ''
        process.stdout.on('data', data => {
            stdoutData += data.toString()
        })
        process.on('close', code => {
            if (code === 0) {
                resolve(stdoutData)
            } else {
                reject(new Error(`exit code ${code}`))
            }
        })

        process.stdin.write(md)
        process.stdin.end()
    })
}
