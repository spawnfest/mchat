var ws = null,
    file, username, 
    start, end,
    SIZE, BYTES_PER_CHUNK,
    chunk, progress;

function init(ip, port) {
  ws = new WebSocket('ws://'+ip+':'+port+'/upload');
  ws.binaryType = 'arraybuffer';
  ws.onopen = function() {
    heartbeat = setInterval(function(){ws.onheartbeat();}, 20000);
    postMessage({status: 'opened'});
  };

  ws.onclose = function() {
    clearInterval(heartbeat);
    postMessage({status: 'closed'});
    close();
  };

  ws.onmessage = function(e) {
    if (e.data instanceof ArrayBuffer) {
      // handle binary data from server
    } else {
      var resp = JSON.parse(e.data);
      if (resp.id === '_continue') {
        if (resp.result.continue) {
          sendSlice();
        } else {
          ws.close();
        }
      } else if (resp.id === '_gotDownloadPid') {
        sendSlice();
      }
    }
  };

  ws.onheartbeat = function() {
    // Timed out
  };
};

function sendSlice() {
  if (start < SIZE) {
    if ('mozSlice' in file) {
      chunk = file.mozSlice(start, end);
    } else {
      chunk = file.webkitSlice(start, end);
    }

    ws.send(chunk);
    start = end;
    end = start + BYTES_PER_CHUNK;

    progress = (end/SIZE) * 100;
    if (progress > 100) progress = 100;
    postMessage({status: 'progress',
                 progress: progress});
  } else {
    postMessage({status: 'progress',
                 progress: 100});
    postMessage({status: 'completed'});
    ws.close();
  }
}

self.addEventListener('message', function(e) {
  if (e.data.cmd === 'init') {
    file = e.data.file;
    username = e.data.username;
    var ip = e.data.ip;
    var port = e.data.port;
    init(ip, port);
  } else if (e.data.cmd === 'start') {
    BYTES_PER_CHUNK = 512 * 1024 // half MB chunk sizes.
    SIZE  = file.size;
    start = progress = 0;
    end   = BYTES_PER_CHUNK;

    if (end > SIZE) {
      end = SIZE
    }

    ws.send(JSON.stringify(
      {method: 'sendToPid',
       jsonrpc: '2.0',
       params: {pid: e.data.pid},
       id: '_sendToPid'}
    ));
  }
}, false);

