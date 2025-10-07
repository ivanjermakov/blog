export type PostMetainfo = {
    title: string
    tags: string[]
    description: string
    date: string
    draft?: boolean
    include?: string[]
    features?: Feature[]
}

export type Feature = 'mermaid' | 'mathjax'

export const postsMeta: Record<string, PostMetainfo> = {
    test: {
        title: 'test post',
        tags: ['meta', 'tag1', 'tag2'],
        description: 'test post',
        date: '1970-01-01',
        features: ['mermaid', 'mathjax'],
        draft: true
    },
    'day-0': {
        title: 'day 0',
        tags: ['meta'],
        description: 'my blog was born here, post number zero',
        date: '2022-02-03'
    },
    'function-coloring-is-inevitable': {
        title: 'Zig’s new I/O: function coloring is inevitable?',
        tags: ['programming', 'language design'],
        description: 'Functions always have color and Zig\'s new I/O won\'t help it',
        date: '2025-07-13',
    },
    'hackable-software': {
        title: 'Hackable software',
        tags: ['programming'],
        description: 'The idea of writing programs for hackers',
        date: '2025-10-07',
        draft: true
    },
    'human-inventions-and-discoveries': {
        title: 'greatest human inventions and discoveries',
        tags: ['history'],
        description: '',
        date: '1970-01-01',
        include: ['index.js', 'data.json'],
        draft: true
    },
    'the-history-of-simple-software': {
        title: 'The history of simple software',
        tags: ['programming'],
        description: 'How software became complex and what we can do about it',
        date: '1970-01-01',
        draft: true
    },
}
