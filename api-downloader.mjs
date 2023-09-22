import * as https from 'https'; // or 'https' for https:// URLs
import * as fs from 'fs';

process.env['NODE_TLS_REJECT_UNAUTHORIZED'] = 0;

var url = process.argv.slice(2)[0];
var target = process.argv.slice(2)[1];
var cred = process.argv.slice(2)[2];

console.log('swagger host: ' + url + ' to file: ' + target);

const file = fs.createWriteStream(target);

var options = {
    host: url,
    port: 443,
    method: 'GET',
    path: '/swagger.json',
    // authentication headers
    headers: {
       'Authorization': 'Basic ' + new Buffer(cred).toString('base64')
    }   
 };

const req = https.get(options, function(response) {
   response.pipe(file);

   // after download completed close file stream
   file.on("finish", () => {
       file.close();
       console.log("Download Completed");
   });
});

req.on('error', (error) => {
   console.log('An error', error);
});