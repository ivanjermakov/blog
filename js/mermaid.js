function initMermaid(matches) {
    const config = {
        theme: matches ? 'dark' : 'default',
        fontFamily: 'inherit',
        sequence: {
            actorFontSize: 18,
            actorFontFamily: 'inherit',
            noteFontFamily: 'inherit',
            messageFontFamily: 'inherit'
        }
    }
    mermaid.initialize(config)
    document.querySelectorAll('div.diagram p').forEach(o => {
        o.style.whiteSpace = 'pre'
        o.style.overflowY = 'auto'
        const insert = code => {
            console.log(code)
            o.innerHTML = code
        }
        try {
            mermaid.render(
                Math.random()
                    .toString(36)
                    .replace(/[^a-z]+/g, ''),
                o.textContent,
                insert
            )
        } catch (e) {
            console.error(e)
            o.innerHTML = e
        }
    })
}

const matches = window.matchMedia('(prefers-color-scheme: dark)').matches
initMermaid(matches)
