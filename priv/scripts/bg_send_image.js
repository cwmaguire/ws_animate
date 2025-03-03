'use strict';

var socket;
var channel;

const RGBA_BYTES_PER_PIXEL = 4;

self.addEventListener('message', dispatch);

function dispatch({data}){
  if(data?.command == 'channel'){
    channel = data.channel;
    socket = create_socket();
  }else if(data?.command == 'send_image'){
    background_send_image(data.animatorName, data.videoFrame);
  }else{
    console.log('bg_send_image unexpected command ...');
    console.dir(data);
  }
}

const copyOptions = {colorSpace: 'srgb', format: 'RGBA'};
var imageArray;
var shrunkArray;

async function background_send_image(animatorName, videoFrame){
  const width = videoFrame.codedWidth;
  const height = videoFrame.codedHeight;
  const rgbSize = width * height * RGBA_BYTES_PER_PIXEL;

  if(!imageArray){
    imageArray = new Uint8ClampedArray(rgbSize);
    shrunkArray = new Uint8ClampedArray(rgbSize / 16);
  }

  const layout = await videoFrame.copyTo(imageArray, copyOptions);
  videoFrame.close();

  for(let y = 0, y2 = 0; y < 480; y += 4, y2++){
    for(let x = 0, x2 = 0; x < (640 * 4); x += 16, x2 += 4){
      for(let component = 0; component < 4; component++){
        shrunkArray[(y2 * 160 * 4) + x2 + component] = imageArray[(y * 640 * 4) + x + component]
      }
    }
  }
  //const binary = imageArray.toString();
  const binary = shrunkArray.toString();
  socket.send(`animator image ${animatorName} ${binary}`);
}

function create_socket(){
  const socket = new WebSocket('ws://localhost:8081/ws');
  socket.addEventListener('open', websocket_open);
  socket.addEventListener('message', websocket_message);
  //socket.addEventListener('error', websocket_error);
  //socket.addEventListener('close', websocket_close);
  return socket;
}

function websocket_open(event){
  socket.send(`channel join ${channel}`);
}

function websocket_message(event){
  console.log('unexpected web worker websocket message');
  console.dir(event);
}
