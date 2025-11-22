import { spawn } from 'child_process'
import { existsSync, mkdirSync } from 'fs'
import { load } from 'cheerio'
import { copyFile, cp, mkdir, readFile, readdir, writeFile } from 'fs/promises'
import { PostMetainfo, postsMeta } from './posts-meta'
import { syntaxHighlight } from './highlight'

const mermaidImport = `\
<script src="https://unpkg.com/mermaid@11.2.1/dist/mermaid.min.js"></script>
<script src="/js/mermaid.js"></script>`
const mathjaxImport =
    '<script type="text/javascript" id="MathJax-script" async src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js"></script>'

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
const outCssDirPath = `${outDirPath}/${cssDirPath}`
console.info(outCssDirPath)
await cp(cssDirPath, outCssDirPath, { recursive: true })

const jsDirPath = 'js'
const outJsDirPath = `${outDirPath}/${jsDirPath}`
console.info(outJsDirPath)
await cp(jsDirPath, outJsDirPath, { recursive: true })

const indexPage = makeIndexPage()
const indexPagePath = `${outDirPath}/index.html`
console.info(indexPagePath)
writeFile(indexPagePath, indexPage)

const rssPage = makeRssFeed()
const rssPagePath = `${outDirPath}/index.xml`
console.info(rssPagePath)
writeFile(rssPagePath, rssPage)

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
            console.info(postPath)
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
    main: templates['404.html']
})
const notFoundPagePath = `${outDirPath}/404.html`
console.info(notFoundPagePath)
writeFile(notFoundPagePath, notFoundPage)

const aboutPage = replaceVariables(templates['index.html'], {
    title: 'ivnj - about',
    description: 'about',
    main: await mdToHtml(templates['about.md'])
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
        main: postItemsFragment
    })
}

async function makePostPage(name: string, metainfo: PostMetainfo, content: string): Promise<string> {
    const tagsFragment = metainfo.tags.map(t => replaceVariables(templates['tag.html'], { title: t })).join('')
    const postFragment = replaceVariables(templates['post.html'], {
        url: `${name}.html`,
        title: metainfo.title,
        date: metainfo.date,
        tags: tagsFragment,
        content
    })
    let head = ''
    if (metainfo.features?.includes('mathjax')) head += mathjaxImport
    if (metainfo.include?.includes('index.js')) head += `<script type="module" src="${name}/index.js"></script>`

    return replaceVariables(templates['index.html'], {
        title: metainfo.title,
        description: metainfo.description,
        main: postFragment,
        body: metainfo.features?.includes('mermaid') ? mermaidImport : '',
        head
    })
}

function makeTagsPage(): string {
    const tagFragments = tags.map(t => replaceVariables(templates['tag.html'], { title: t }))
    const tagsFragment = replaceVariables(templates['tags.html'], { tag: tagFragments.join('') })
    return replaceVariables(templates['index.html'], {
        title: 'ivnj blog',
        description: 'ivnj blog',
        main: tagsFragment
    })
}

function replaceVariables(template: string, vars: Record<string, string>): string {
    const templated = Object.entries(vars).reduce((t, [vk, v]) => t.replaceAll(`$${vk}$`, v), template)
    const cleaned = templated.replaceAll(/\$.*\$/g, '')
    return cleaned
}

async function mdToHtml(md: string): Promise<string> {
    const html = await new Promise<string>((resolve, reject) => {
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
    const query = load(html)
    for (const e_ of query('pre code')) {
        const e = query(e_)
        const code = query(e).text()
        const lang = (e.attr('class') ?? '')
            .split(' ')
            .filter(c => c !== 'sourceCode')
            .at(0)
        if (lang) {
            const highlit = await syntaxHighlight(lang, code)
            e.empty().append(highlit)
        }
    }
    return query.html()
}

function makeRssFeed(): string {
    const now = new Date().toUTCString()
    const items = Object.entries(postsMeta)
        .filter(([, pm]) => pm.draft !== true)
        .map(([name, pm]) => {
            const link = `https://blog.ivnj.org/post/${name}.html`
            return `\
        <item>
            <title>${pm.title}</title>
            <link>${link}</link>
            <guid>${link}</guid>
            <pubDate>${new Date(pm.date).toUTCString()}</pubDate>
        </item>`
        })
        .join('\n')
    return `\
<rss version="2.0">
	<channel>
		<title>Ivan Ermakov's blog</title>
		<link>https://blog.ivnj.org/</link>
		<description>Recent posts on Ivan Ermakov's blog</description>
		<generator>https://github.com/ivanjermakov/blog</generator>
		<language>en-us</language>
		<lastBuildDate>${now}</lastBuildDate>
${items}
	</channel>
</rss>`
}
