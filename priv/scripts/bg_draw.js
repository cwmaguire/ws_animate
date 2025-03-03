'use strict';

var canvas;
var context2dWithDims;

var receiveBuffer = [];
var drawBuffer = [];
var clickTargets = [];

var transformSerialized;

const drawCache = new Map();
const imageCache = new Map();
const videoReady = new Map();
const images = new Map();

self.addEventListener('message', dispatch);

function dispatch({data}){
  if(data?.command == 'animate'){
    render_buffer();
  }else if(data?.command == 'buffer'){
    buffer_command(data.obj);
  }else if(data?.command == 'transfer canvas'){
    setup_canvas(data);
  }else if(data?.command == 'video is ready'){
    videoReady.set(data.deviceId, true);
  }else if('image' in data){
    images.set(data.src, data.image);
  }else if(data?.command == 'get click target'){
    get_click_target(data.event);
  }else if(data?.command == 'echo'){
    console.log(`bg_draw echo: ${data?.text}`);
  }
}

function buffer_command(obj){
  if(obj.cmd == 'finish'){
    drawBuffer = receiveBuffer;
    receiveBuffer = [];
  }else if(maybe_wait_image(obj) &&
           maybe_wait_video(obj)){
    receiveBuffer.push(obj);
  }else{
    console.log(`bg_draw: not ready for ...`);
    console.log(obj);
  }
}

function render_buffer(_timestamp){
  const buffer = drawBuffer;
  drawBuffer = [];
  buffer.forEach(draw);
}

function draw(command){
  maybe_cache(command);
  const {cmd} = command;
  switch(cmd){
    case 'clear':
      clear_canvas(context2dWithDims);
      break;
    case 'square':
      square(context2dWithDims, command);
      break;
    case 'square_filled':
      square_filled(context2dWithDims, command);
      break;
    case 'square_gradient':
      square_gradient(context2dWithDims, command);
      break;
    case 'circle':
      circle(context2dWithDims, command);
      break;
    case 'line':
      line(context2dWithDims, command);
      break;
    case 'text':
      text(context2dWithDims, command);
      break;
    case 'image':
      image(context2dWithDims, command);
      break;
    case 'bitmap':
      bitmap(context2dWithDims, command);
      break;
    case 'video_frame':
      video_frame(context2dWithDims, command);
      break;
    case 'transform':
      transform(context2dWithDims, command);
      break;
    case 'cached':
      console.dir(command);
      const cachedCommand = drawCache.get(command.id);
      if(cachedCommand){
        draw(drawCache.get(command.id));
      }else{
        console.log(`draw cached: Could not find cached command for ${command.id}`);
        console.dir(drawCache);
      }
      break;
    default:
      console.log(`Ignoring command ${cmd}`);
  }
}

function maybe_cache(command){
  if(command?.shouldCache){
    console.log('caching');
    drawCache.set(command.id, {...command, shouldCache: false})
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
  if('stroke_style' in command){
    ctx.strokeStyle = command.stroke_style;
    ctx.strokeRect(x, y, w, h);
  }
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

function text({ctx}, {text, x, y, font_size, font, font_color}){
  ctx.font = `${font_size} ${font}, monospace`;
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
  // XXX not allowed to draw to canvas since it's owned
  // by a worker thread
  // ctx.drawImage(img, imageSourceX, imageSourceY, img.width, img.height, x, y, imageWidth, imageHeight);
  add_click_target({...command, type: 'square', w: imageWidth, h: imageHeight});
}

function bitmap({ctx}, command){
  let imageData;
  const {shouldCache, x, y} = command;
  if(shouldCache){
    const {w, data} = command;
    const array = new Uint8ClampedArray(data);
    imageData = new ImageData(array, w);
    imageCache.set(command.id, imageData);
  }else{
    imageData = imageCache.get(command.id);
  }

  ctx.putImageData(imageData, x, y);
  //add_click_target({...command, type: 'square', w: imageWidth, h: imageHeight});
}

async function video_frame({ctx}, command){
  const {x, y, w, h} = command;
  ctx.drawImage(video, x, y, w, h);
  add_click_target({...command, type: 'square'});
}

function transform({ctx}, command){
  const {transform: abcdef} = command;
  const transformSerializedLocal = abcdef.toString();
  if(transformSerializedLocal != transformSerialized){
    console.log(`Setting transform to ${abcdef}`);
    transformSerialized = transformSerializedLocal;
    const [a, b, c, d, e, f] = abcdef;
    ctx.setTransform(a, b, c, d, e, f);
    ctx.translate(400, 0);
    ctx.scale(0.5, 0.5);
  }
}

function add_click_target(shape){
  clickTargets.unshift(shape);
}

function get_click_target({x, y}){
  const tempClickTargets = clickTargets.slice();
  const isClickTargetFun = (ct) => is_click_target(ct, {x, y});
  const maybeClickTarget = tempClickTargets.filter(isClickTargetFun)[0];
  if(maybeClickTarget){
    const command =
      {target: 'main',
       command: 'click target',
       clickTarget: maybeClickTarget};
    self.postMessage(command);
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

function maybe_wait_video(drawCommand){
  const {cmd, device_id: deviceId} = drawCommand;
  if(cmd == 'video_frame' && !videoReady.has(deviceId)){
    trigger_video(deviceId);
    return false;
  }else{
    return true;
  }
}

function trigger_video(deviceId){
  self.postMessage({target: 'main',
                    command: 'wait video',
                    deviceId});
}

function maybe_wait_image(drawCommand){
  const {cmd, src} = drawCommand;
  if(cmd == 'image' && !imageReady.has(src)){
    trigger_image
    return false;
  }else{
    return true;
    // TODO move to draw commands
    drawCommand.img = loadedImages.get(src);
  }
}

function trigger_image(drawCommand){
  self.postMessage({target: 'main',
                    command: 'wait image',
                    url: drawCommand.src});
}

function setup_canvas({canvas: canvas_,
                       shouldAddClickTargeting = false}){
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
}
