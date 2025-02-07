// @ts-check
export {}

window.addEventListener('load', async () => {
    const data = await (await fetch('human-inventions-and-discoveries/data.json')).json()
    const eventsElement = document.querySelector('#events')
    data.forEach(e => {
        const div = document.createElement('div')
        div.innerHTML = e.html
        eventsElement?.appendChild(div)
    })
})
