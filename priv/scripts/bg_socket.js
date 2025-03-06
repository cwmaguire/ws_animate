'use strict';

var channel;

const drawWorker = new Worker('bg_draw.js');
const socket = create_socket();

self.addEventListener('message', route)
drawWorker.addEventListener('message', route)

self.requestAnimationFrame(animate);

function animate(){
  const command =
    {target: 'draw',
     command: 'animate'};
  drawWorker.postMessage(command);
  self.requestAnimationFrame(animate);
}

function create_socket(){
  const socket = new WebSocket('ws://localhost:8081/ws');
  socket.addEventListener('open', websocket_open);
  socket.addEventListener('message', websocket_message);
  socket.addEventListener('error', websocket_error);
  socket.addEventListener('close', websocket_close);
  return socket;
}

function websocket_open(event){
  socket.send('channel start');
  socket.send('channel sub draw');
  socket.send('channel sub info');
  socket.send('animator list');
  socket.send('registry sub channels');
}

function route({data}){
  if(data?.target == 'draw' && 'transfer' in data){
    drawWorker.postMessage(data, data.transfer);
  }else if(data?.target == 'server'){
    socket.send(data.command);
  }else if(data?.target == 'draw'){
    drawWorker.postMessage(data);
  }else if(data?.target == 'main'){
    self.postMessage(data);
  }else{
    dispatch(data);
  }
}

function dispatch(data){
  if(data?.command == 'switch channel'){
    switch_channel(data);
  }else{
    console.log('bg_socket unexpected command ...');
    console.dir(data);
  }
}

function websocket_message(event){
  let obj = JSON.parse(event.data);
  switch(obj.type){
    case 'draw':
      drawWorker.postMessage({command: 'buffer', obj});
      break;
    case 'info':
      self.postMessage(obj);
      break;
    case 'log':
      console.log('Server: ' + obj.log);
      break;
  }
}

function websocket_error(event){
  console.log('Websocket error: ', event);
}

function websocket_close(event){
  console.log('Websocket close. Code: ', event.code, '. Reason: \'', event.reason, '\'. Clean? ', event.wasClean);
}

function switch_channel(channel){
  socket.send('channel leave');
  socket.send(`channel join ${channel}`);
  socket.send(`channel sub draw`);
  socket.send('channel sub info');
  socket.send('animator list');
}
