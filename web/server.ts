const root = `${import.meta.dir}/..`
const app = Bun.serve({
  async fetch(req) {
    const url = new URL(req.url);
    if (url.pathname === "/") {
      await Bun.build({
        entrypoints: [`${import.meta.dir}/webentry.ts`],
        outdir: './build',
      });

      return new Response(`<!DOCTYPE html><html><body><script src="webentry.js"></script></body></html>`, {
        headers: {
          "Content-Type": "text/html",
        },
      });
    }
    if (url.pathname === "/webentry.js") 
      return new Response(Bun.file(`${root}/build/webentry.js`));
    return new Response("404!");
  },
});

console.log(`http://${app.hostname}:${app.port}`)