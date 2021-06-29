var http = require('http');
const { callbackify } = require('util');

main();

async function main() {
  var cid = await activate(1);

  await callEndpoint(cid, "deposit", {
    "deposit'amount": 25,
    "deposit'asset": [
      {
        "unCurrencySymbol": "e201a404f0ff5bf1aae5711a6081193ac4589d33f629656fc749c4f3e798df0f"
      },
      {
        "unTokenName": "Dollar"
      }
    ],
  });

  // await callEndpoint(cid, "borrow", {
  //   "borrow'amount": 10,
  //   "borrow'asset": [
  //     {
  //       "unCurrencySymbol": "Dollar"
  //     },
  //     {
  //       "unTokenName": "Dollar"
  //     }
  //   ],
  //   "borrow'rate": 0
  // });
}

async function activate(wallet) {
  var rawCid = await post('/api/new/contract/activate', {
    "caID": "User",
    "caWallet": {
        "getWallet": wallet
    }
  });

  return rawCid.unContractInstanceId;
}

function callEndpoint(cid, endpoint, reqBody) {
  post(`/api/new/contract/instance/${cid}/endpoint/${endpoint}`, reqBody);
}

function post(path, reqBody) {
  return new Promise((resolve, reject) => {
    var req = http.request({
      'method': 'POST',
      'hostname': 'localhost',
      'port': 8080,
      'path': path,
      'headers': {
        'Content-Type': 'application/json'
      },
      'maxRedirects': 20
    }, function (res) {
      var chunks = [];
    
      res.on("data", function (chunk) {
        chunks.push(chunk);
      });
    
      res.on("end", function (chunk) {
        var body = Buffer.concat(chunks);
        var decoded = JSON.parse(body);
        console.log("Response: \n" + body.toString() + '\n');
        resolve(decoded);
      });
    
      res.on("error", function (error) {
        console.error(error);
        reject(error);
      });
    });
    
    var postData = JSON.stringify(reqBody);
    
    console.log("Sending POST request to: " + path + "\n");
    console.log("Request Body:\n" + JSON.stringify(reqBody) + "\n");

    req.write(postData);
    
    req.end();
  });
}