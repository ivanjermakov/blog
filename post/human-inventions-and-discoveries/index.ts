import { existsSync } from 'fs'
import { readFile, writeFile } from 'fs/promises'
import { JSDOM } from 'jsdom'

type HistoricalEvent = {
    year: number
    html: string
}

const parseYear = (raw: string, bc = false): number | undefined => {
    const clean = raw
        .replace(/[,:]/, '')
        .replace(/(By at least )|(By )|(Before )|(After )|(Late )/, '')
        .trim()
    const range = clean.split(/ [–-] /)
    bc ||= clean.endsWith(' BC')
    if (range.length === 2) {
        return parseYear(range[0], bc)
    } else if (clean.match(/^\d+( BC)?$/)) {
        return (bc ? -1 : 1) * Number.parseInt(clean)
    } else if (clean.match(/^\d+s?$/)) {
        return (bc ? -1 : 1) * Number.parseInt(clean)
    } else if (clean.includes('century')) {
        return (bc ? -1 : 1) * 1e2 * Number.parseInt(clean.match(/\d+/)![0])
    } else if (clean.includes('millennium')) {
        return (bc ? -1 : 1) * 1e3 * Number.parseInt(clean.match(/\d+/)![0])
    } else if (clean.includes('kya')) {
        return 2e3 - 1e3 * Number.parseInt(clean.match(/\d+/)![0])
    } else if (clean.includes('Mya')) {
        return -1e6 * Number.parseFloat(clean.match(/\d+(.\d+)?/)![0])
    }
    console.warn('year parse error', raw)
    return undefined
}

const pagePath = '/tmp/Timeline_of_historic_inventions.html'
const resultPath = 'post/human-inventions-and-discoveries/data.json'

let page: string
if (existsSync(pagePath)) {
    page = (await readFile(pagePath)).toString()
} else {
    page = await (await fetch('https://en.wikipedia.org/wiki/Timeline_of_historic_inventions')).text()
    await writeFile(pagePath, page)
}

const document = new JSDOM(page).window.document
const lis = [...document.querySelectorAll('#bodyContent > #mw-content-text > .mw-content-ltr > ul')]
    .filter(e => e.firstChild?.firstChild?.nodeName === 'B')
    .flatMap(ul => [...ul.children].map(li => li))
const events: HistoricalEvent[] = lis.map(li => {
    const yearRaw = li.firstChild!.textContent!
    return {
        year: parseYear(yearRaw)!,
        html: li.innerHTML
    }
})

await writeFile(resultPath, JSON.stringify(events))
