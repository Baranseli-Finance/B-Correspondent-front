import * as esbuild from 'esbuild';
import filesize from 'filesize'; 
import { statSync } from 'fs';

function parseHrtimeToSeconds(hrtime) {
    var seconds = (hrtime[0] + (hrtime[1] / 1e9)).toFixed(3);
    return seconds;
};

var startTime = process.hrtime();

await esbuild.build({
  entryPoints: ['./output/Main/index.js'],
  bundle: true,
  outfile: 'app/app.js',
  minify: true
}).then(_ => { 
    var elapsedSeconds = parseHrtimeToSeconds(process.hrtime(startTime));
    const {size} = statSync('app/app.js');
    const fileSizeInMb = filesize(size, {round: 0});
    console.log('\x1b[44m', 'build has been completed, dist/app.js: ' + fileSizeInMb + ', build time: ' + elapsedSeconds + 's', '\x1b[0m');
});