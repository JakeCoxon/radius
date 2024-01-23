import { readdir } from "node:fs/promises";

const root = `${import.meta.dir}/..`
const app = Bun.serve({
  async fetch(req) {
    const url = new URL(req.url)
    if (url.pathname === '/') {
      try {
        const res = await Bun.build({
          entrypoints: [`${import.meta.dir}/webentry.tsx`],
          outdir: './build',
        })
        if (!res.success) console.log(res)
      } catch (ex) {
        console.error(ex)
      }
      let files = await readdir(`${root}/tests/fixtures`);
      files.sort()
      files = files.filter(x => x.endsWith('.rad'))

      return new Response(`<!DOCTYPE html><html><body>
      <script>globalThis.FILES = ${JSON.stringify(files)}</script>
      <script src="webentry.js"></script></body></html>`, {
        headers: {
          'Content-Type': 'text/html',
        },
      })
    }
    if (url.pathname.startsWith('/fixtures/')) {
      return new Response(Bun.file(`${root}/tests/${url.pathname}`))
    }
    return new Response(Bun.file(`${root}/build/${url.pathname}`))
    // return new Response("404!");
  },
})

console.log(`http://${app.hostname}:${app.port}`)
