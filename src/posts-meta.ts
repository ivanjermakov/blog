export type PostMetainfo = {
    title: string
    tags: string[]
    description: string
    date: string
    draft?: boolean
    include?: string[]
}

export const postsMeta: Record<string, PostMetainfo> = {
    test: {
        title: 'test post',
        tags: ['meta', 'tag1', 'tag2'],
        description: 'test post',
        date: '1970-01-01',
        draft: true
    },
    'day-0': {
        title: 'day 0',
        tags: ['meta'],
        description: 'my blog was born here, post number zero',
        date: '2022-02-03'
    },
    'human-inventions-and-discoveries': {
        title: 'greatest human inventions and discoveries',
        tags: ['history'],
        description: '',
        date: '2025-01-07',
        include: ['index.js', 'data.json'],
        draft: true
    },
    'function-coloring-is-inevitable': {
        title: 'Zig’s new I/O: function coloring is inevitable?',
        tags: ['programming', 'language design'],
        description: 'Functions always have color and Zig\'s new I/O won\'t help it',
        date: '2025-13-07'
    },
}
