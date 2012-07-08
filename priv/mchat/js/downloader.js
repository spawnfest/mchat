var ws = null,
    file, username,
    start, end,
    SIZE, BYTES_PER_CHUNK,
    progress = 0, fs = null, fileWriter = null;

function init(ip, port) {
  ws = new WebSocket('ws://'+ip+':'+port+'/download');
  ws.binaryType = "arraybuffer";
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
      if (fs === null) {
        initFileSystem();
      }
      downloadSlice(e.data);
    } else {
      var resp = JSON.parse(e.data);
      if (resp.id === "_getPid") {
        postMessage({status: "download", pid: resp.result.pid});
      } else {
        postMessage({status: "error", msg: resp.result});
      }
    }
  };

  ws.onheartbeat = function() {
    // Timed out
  };

}

function initFileSystem() {
  fs = self.webkitRequestFileSystemSync(TEMPORARY,
                                        1024*1024*500 /*500MB*/);
  fileEntry = fs.root.getFile(file.name, {create: true});
  fileWriter = fileEntry.createWriter();
}

function downloadSlice(data) {
  var bb = new WebKitBlobBuilder();
  bb.append(data);
  fileWriter.write(bb.getBlob(file.type));
  progress = (fileWriter.position/file.size) * 100;
  postMessage({status: 'progress', progress: progress});
  
  if (fileWriter.position == file.size) {
    postMessage({status: 'completed'});
    ws.close();
  } else {
    ws.send(JSON.stringify({method: 'continue', 
                            jsonrpc: '2.0',
                            params: {answer: true}}));
  }
}

self.addEventListener('message', function(e) {
  if (e.data.cmd === 'init') {
    init(e.data.ip, e.data.port);
  } else if (e.data.cmd === 'start') {
    file = e.data.file;
    ws.send(JSON.stringify(
      {method: 'getPid', jsonrpc: '2.0', id: '_getPid'}
    ));
  }
}, false);


