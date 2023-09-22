import * as esbuild from 'esbuild';
import http from 'node:http';

// Start esbuild's server on a random local port
let ctx = await esbuild.context({
  // ... your build options go here ...
  entryPoints: ['index.js'],
  bundle: true,
  outfile: 'app/app.js'
});

// The return value tells us where esbuild's local server is
let { host, port } = await ctx.serve({ servedir: 'app', port: 8080, host: "127.0.0.1"});

console.log('\x1b[44m', 'server has been started: ' + 'http://' + host + ':' + port, '\x1b[0m');

// Then start a proxy server on port 3000
http.createServer((req, res) => {
  const options = {
    hostname: host,
    port: port,
    path: req.url,
    method: req.method,
    headers: req.headers
  };

  // Forward each incoming request to esbuild
  const proxyReq = http.request(options, proxyRes => {
    proxyRes.pipe(res, { end: true })
  })

  // Forward the body of the request to esbuild
  req.pipe(proxyReq, { end: true });
}).listen(3000);
