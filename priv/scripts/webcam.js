'use strict';


// https://developer.mozilla.org/en-US/docs/Web/API/VideoFrame/format
//
// NV12:
//   2 "planes": Y (luma) + UV (Chroma U, Chroma V)
//   Luma: 8 bits per pixel
//   Chroma U: 8 bits per 4 pixels (1 sample per four pixels)
//   Chroma V: 8 bits per 4 pixels (1 sample per four pixels)
//   Works out to 1.5 bytes per pixel
//   (versus RBGA with 4 bytes per pixel)
const NV12 = 'NV12';

async function capture_image_binary(animatorName, videoElement){
  const videoFrame = new VideoFrame(videoElement);
  worker.postMessage({animatorName, videoFrame});
}

function draw_image_data(imageData, x, y, ctx){
  ctx.putImageData(imageData, x, y);
}

// Formats:
// https://developer.mozilla.org/en-US/docs/Web/API/VideoFrame/format
function get_video_frame_format(videoFrame){
  return videoFrame.format;
}

function draw_video_frame(videoFrame){
  ctx.drawImage(videoFrame, 0, 0);
}

// unnecessary:
function image_data_to_bitmap(imageData){
  let bitmap;
  window.createImageBitmap(imageData).then((x) => bitmap = x);
  return bitmap;
}

// (unnecessary)
function draw_bitmap_on_canvas(bitmap, canvas2dContext){
  canvas2dContext.drawImage(bitmap, 400, 400);
}

function get_devices(){
  navigator.mediaDevices.enumerateDevices().then((x) => devices = x).catch((err) => console.log(err));
}

// (6) [InputDeviceInfo, InputDeviceInfo, InputDeviceInfo, InputDeviceInfo, InputDeviceInfo, MediaDeviceInfo]
// 0: InputDeviceInfo {deviceId: '', kind: 'audioinput', label: '', groupId: ''}
// 1: InputDeviceInfo {deviceId: 'c6bb56ab36e27b2f49459d25ea6a74b329b78c64ebb2b305bbdb804de920d3eb', kind: 'videoinput', label: 'USB2.0 PC CAMERA (1908:2310)', groupId: '5339e1784dd7ab5c2909748037c82d5bc85bfb8d79a5067e048358441255b446'}
// 2: InputDeviceInfo {deviceId: 'b35c1ea89a4fb9423da735fef3c9aeddc1745b4c408bef58109c2eb02bb9f6eb', kind: 'videoinput', label: 'Webcam C170 (046d:082b)', groupId: '6790e5f7d4068e128b8424a041ae3fc971987bef534e6279767e32bfbfce8377'}
// 3: InputDeviceInfo {deviceId: 'cde819aad3a5f7da8759721271d1d3deaf3dbdce8666ecfb5f4180f93f5e0d00', kind: 'videoinput', label: 'USB2.0 PC CAMERA (1908:2311)', groupId: '5339e1784dd7ab5c2909748037c82d5bc85bfb8d79a5067e048358441255b446'}
// New Logitech
// 4: InputDeviceInfo {deviceId: '0d188ab134e1cfbe5d0d6dd57ce8f318e69e49ada333950a0cc934942f712bed', kind: 'videoinput', label: 'UVC Camera (046d:081b) (046d:081b)', groupId: '8ba196b87edfccfbcc6c76170b35cb2134ca6077db8f229ac949c024dff4b56e'}
// 5: MediaDeviceInfo {deviceId: '', kind: 'audiooutput', label: '', groupId: ''}length: 6[[Prototype]]: Array(0)

