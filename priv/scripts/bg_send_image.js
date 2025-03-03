'use strict';

var socket;
var channel;

const RGBA_BYTES_PER_PIXEL = 4;

self.addEventListener('message', dispatch);

function dispatch({data}){
  if('channel' in data){
    channel = data.channel;
    socket = create_socket();
  }else{
    background_send_image(data);
  }
}

async function background_send_image({animatorName, videoFrame}){
  const format = videoFrame.format;
  const width = videoFrame.codedWidth;
  const height = videoFrame.codedHeight;
  const rgbSize = width * height * RGBA_BYTES_PER_PIXEL;

  const imageArray = new Uint8ClampedArray(rgbSize);
  const copyOptions = {colorSpace: 'srgb', format: 'RGBA'};
  const layout = await videoFrame.copyTo(imageArray, copyOptions);
  videoFrame.close();
  const binary = imageArray.toString();
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
