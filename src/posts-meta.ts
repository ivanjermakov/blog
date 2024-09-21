export type PostMetainfo = {
    title: string
    tags: string[]
    description: string
    date: string
    draft?: boolean
}

export const postsMeta: Record<string, PostMetainfo> = {
    'day-0': {
        title: 'day 0',
        tags: ['meta'],
        description: 'my blog was born here, post number zero',
        date: '2022-02-03'
    },
    test: {
        title: 'test post',
        tags: ['meta', 'tag1', 'tag2'],
        description: 'test post',
        date: '1970-01-01',
        draft: true
    }
}
