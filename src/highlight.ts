import { ThemeInput, createHighlighter } from 'shiki'

const scopes = ['comment', 'keyword', 'string', 'number']
const theme: ThemeInput = {
    name: 'theme',
    fg: 'var(--fg)',
    settings: scopes.map(scope => ({
        scope: [scope],
        settings: {
            foreground: `var(--${scope})`
        }
    }))
}

export async function syntaxHighlight(lang: string, code: string): Promise<string> {
    const highlighter = await createHighlighter({
        themes: [theme],
        langs: ['zig', 'typescript', 'javascript', 'haskell', 'bash']
    })
    return highlighter.codeToHtml(code, { lang, theme: 'theme', structure: 'inline' })
}
