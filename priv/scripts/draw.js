'use strict';
var canvas;
var context2dWithDims;
var receiveBuffer = [];
var drawBuffer = [];
var clickTargets = [];
var video;
var stream;
const loadedImages = new Map;
const PATH_TO_IMAGES = 'images/';

function setup_canvas(canvas_, shouldAddClickTargeting = false){
  canvas = canvas_;
  const ctx = canvas.getContext('2d');

  // Matrix transform from DOMMatrix
  // a c e     x
  // b d f  .  y    a = x scaling
  // - - 1     1    d = y scaling

  // Multiple each row element by each column
  // element and add them together to get the final
  // row element: e.g. ax + cy + e1 = x2
  //                   bx + dy + f1 = y2
  //
  // See https://tinylittlemaggie.github.io/transformation-matrix-playground/

  const {a,d} = ctx.getTransform();
  const xScale = a;
  const yScale = d;
  context2dWithDims = {ctx: ctx,
                       w: canvas.width / xScale,
                       h: canvas.height / yScale};
  if(shouldAddClickTargeting){
    canvas.addEventListener('click', get_click_target);
  }
}

requestAnimationFrame(animation_frame);

function buffer_command(obj){
  if(obj.cmd == 'finish'){
    drawBuffer = receiveBuffer;
    receiveBuffer = [];
  }else{
    maybe_wait_image(obj);
    maybe_wait_video(obj);
    receiveBuffer.push(obj);
  }
}

async function maybe_wait_image(drawCommand){
  const {cmd, src} = drawCommand;
  if(cmd == 'image' && !loadedImages.has(src)){
    const img = document.createElement('img');
    console.time(`load image ${src}`);
    await load_image(img, PATH_TO_IMAGES + src);
    console.timeEnd(`load image ${src}`);
    loadedImages.set(src, img);
    drawCommand.img = img;
  }else if(cmd == 'image'){
    drawCommand.img = loadedImages.get(src);
  }
}


function load_image(img, url) {
  return new Promise((resolve, reject) => {
    img.onload = () => resolve(img);
    img.onerror = reject;
    img.src = url;
  });
}

async function maybe_wait_video(drawCommand){
  const {cmd, device_id: deviceId} = drawCommand;
  if(cmd == 'video_frame' && !video){
    video = document.createElement('video');
    console.time(`play video ${deviceId}`);
    await play_video(deviceId);
    console.timeEnd(`play video ${deviceId}`);
  }
}

function play_video(deviceId){
  const videoConstraint = {video: {deviceId: deviceId}};
  navigator.mediaDevices.getUserMedia(videoConstraint)
    .then((stream_) =>
      {stream = stream_;
       video.srcObject = stream;
       video.play()})
    .then(() => console.log(`playing video from ${deviceId}`));
}

function animation_frame(_timestamp){
  render_buffer();
  requestAnimationFrame(animation_frame);
}

function render_buffer(_timestamp){
  const buffer = drawBuffer;
  drawBuffer = [];
  buffer.forEach(draw);
}

function draw(Command){
  const {cmd} = Command;
  switch(cmd){
    case 'clear':
      clear_canvas(context2dWithDims);
      break;
    case 'square':
      square(context2dWithDims, Command);
      break;
    case 'square_filled':
      square_filled(context2dWithDims, Command);
      break;
    case 'square_gradient':
      square_gradient(context2dWithDims, Command);
      break;
    case 'circle':
      circle(context2dWithDims, Command);
      break;
    case 'line':
      line(context2dWithDims, Command);
      break;
    case 'text':
      text(context2dWithDims, Command);
      break;
    case 'image':
      image(context2dWithDims, Command);
      break;
    case 'video_frame':
      video_frame(context2dWithDims, Command);
      break;
    default:
      console.log(`Ignoring command ${cmd}`);
  }
}

function clear_canvas({ctx, w, h}){
  ctx.clearRect(0, 0, w, h);
  clickTargets = [];
}

function square({ctx}, command){
  const {x, y, w, h, style, name} = command;
  ctx.strokeSyle = style;
  ctx.strokeRect(x, y, w, h);
  add_click_target({...command, type: 'square'});
}

function square_filled({ctx}, command){
  const {x, y, w, h, style, name} = command;
  ctx.fillStyle = style;
  ctx.fillRect(x, y, w, h);
  add_click_target({...command, type: 'square'});
}

function square_gradient({ctx}, command){
  var {x, y, w, h, style : {gx1, gy1, gx2, gy2, stop1: {stop: s1, color: c1}, stop2: {stop: s2, color: c2}}} = command;
  var gradient = ctx.createLinearGradient(gx1, gy1, gx2, gy2);
  gradient.addColorStop(s1, c1);
  gradient.addColorStop(s2, c2);
  ctx.fillStyle = gradient;
  ctx.fillRect(x, y, w, h);
  add_click_target({...command, type: 'square'});
}

function circle({ctx}, command){
  const {x, y, r, style, name} = command;
  //console.log(`square: x: ${x}, y: ${y}, w: ${w}, h: ${h}, style: ${style}`);
  ctx.strokeSyle = style;
  ctx.beginPath();
  ctx.ellipse(x, y, r, r, 0, 0, 2 * Math.PI);
  ctx.stroke();
  add_click_target({...command, type: 'circle'});
}

function line({ctx}, command){
  const {x1, y1, x2, y2} = command;
  ctx.strokStyle = 'black';
  ctx.beginPath();
  ctx.moveTo(x1, y1);
  ctx.lineTo(x2, y2);
  ctx.stroke();
}

function text({ctx}, {text, x, y, font_size, font_color}){
  ctx.font = `${font_size}, 'Courier New', monospace;`;
  ctx.fillStyle = font_color;
  ctx.fillText(text, x, y);
  // TODO add click target
  // get calculated font width and height from context2d
}

function line({ctx}, command){
  const {x1, y1, x2, y2} = command;
  ctx.strokStyle = 'black';
  ctx.beginPath();
  ctx.moveTo(x1, y1);
  ctx.lineTo(x2, y2);
  ctx.stroke();
}

async function image({ctx}, command){
  const {img, x, y, w_scale: widthScale, h_scale: heightScale, name} = command;
  const imageHeight = img.height * heightScale;
  const imageWidth = img.width * widthScale;
  const imageSourceX = 0;
  const imageSourceY = 0;
  ctx.drawImage(img, imageSourceX, imageSourceY, img.width, img.height, x, y, imageWidth, imageHeight);
  add_click_target({...command, type: 'square', w: imageWidth, h: imageHeight});
}

async function video_frame({ctx}, command){
  const {x, y, w, h} = command;
  ctx.drawImage(video, x, y, w, h);
  add_click_target({...command, type: 'square'});
}

function add_click_target(shape){
  clickTargets.unshift(shape);
}

function get_click_target(event){
  const tempClickTargets = clickTargets.slice();
  const {offsetX: x, offsetY: y} = event;
  const isClickTargetFun = (ct) => is_click_target(ct, {x, y});
  const maybeClickTarget = tempClickTargets.filter(isClickTargetFun)[0];
  if(maybeClickTarget){
    document.querySelector(`#animator_controls_button_${maybeClickTarget.name}`).click();
  }
}

function is_click_target(shape, {x, y}){
  switch(shape.type){
    case 'square':
      return square_hittest({x, y}, shape);
      break;
    case 'circle':
      return circle_hittest({x, y}, shape);
      break;
  }
}

function square_hittest({x, y}, square){
  return x >= square.x &&
         x <= square.x + square.w &&
         y >= square.y &&
         y <= square.y + square.h;
}

function circle_hittest({x: x1, y: y1}, {x: x2, y: y2, r}){
  return r >= Math.hypot(x1 - x2, y1 - y2);
}
