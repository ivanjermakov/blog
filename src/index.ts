import { spawn } from 'child_process'
import { cp, mkdir, readFile, readdir, writeFile } from 'fs/promises'
import { PostMetainfo, postsMeta } from './posts-meta'

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

const indexHtmlPage = makeIndexPage()
const indexPagePath = `${outDirPath}/index.html`
console.info(indexPagePath)
writeFile(indexPagePath, indexHtmlPage)

const postDirPath = 'post'
const outPostDirPath = `${outDirPath}/post`
await mkdir(outPostDirPath, { recursive: true })
await Promise.all(
    Object.entries(postsMeta).map(async ([pName, pm]) => {
        const postMd = (await readFile(`${postDirPath}/${pName}.md`)).toString()
        const postPage = await makePostPage(pName, pm, postMd)
        const postPath = `${outPostDirPath}/${pName}.html`
        console.info(postPath)
        await writeFile(postPath, postPage)
    })
)

const notFoundPage = replaceVariables(templates['index.html'], {
    title: '404',
    description: 'not found',
    body: templates['404.html']
})
const notFoundPagePath = `${outDirPath}/404.html`
console.info(notFoundPagePath)
writeFile(notFoundPagePath, notFoundPage)

function makeIndexPage(): string {
    const postItemsFragment = Object.entries(postsMeta)
        .filter(([, pm]) => pm.draft !== true)
        .map(([pName, pm]) => {
            const tagsFragment = pm.tags.map(t => templates['tag.html'].replaceAll('$title$', t)).join('')

            return replaceVariables(templates['post-item.html'], {
                url: `post/${pName}.html`,
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

async function makePostPage(name: string, metainfo: PostMetainfo, md: string): Promise<string> {
    const postContent = await mdToHtml(md)
    const tagsFragment = metainfo.tags.map(t => templates['tag.html'].replaceAll('$title$', t)).join('')
    const postFragment = replaceVariables(templates['post.html'], {
        url: `${postDirPath}/${name}.html`,
        title: metainfo.title,
        date: metainfo.date,
        tags: tagsFragment,
        body: postContent
    })
    return replaceVariables(templates['index.html'], {
        title: 'ivnj blog',
        description: 'ivnj blog',
        body: postFragment
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
