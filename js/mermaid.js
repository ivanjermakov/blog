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

const switchTheme = matches => {
    if (matches) {
        document.querySelector('link[title=dark]').removeAttribute('disabled')
        document.querySelector('link[title=light]').setAttribute('disabled', 'disabled')
    } else {
        document.querySelector('link[title=light]').removeAttribute('disabled')
        document.querySelector('link[title=dark]').setAttribute('disabled', 'disabled')
    }
}
const query = '(prefers-color-scheme: dark)'
const matches = window.matchMedia(query).matches
switchTheme(matches)
window.matchMedia(query).addEventListener('change', event => switchTheme(event.matches))

initMermaid(matches)
