'use strict';
var canvas;
var context2dWithDims;
var receive_buffer = [];
var draw_buffer = [];
var click_targets = [];

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
    draw_buffer = receive_buffer;
    receive_buffer = [];
  }else{
    receive_buffer.push(obj);
  }
}

function animation_frame(_timestamp){
  render_buffer();
  requestAnimationFrame(animation_frame);
}

function render_buffer(_timestamp){
  const buffer = draw_buffer;
  draw_buffer = [];
  buffer.forEach(draw);
}

function draw(Command){
  const {cmd} = Command;
  switch(cmd){
    case 'clear':
      clear(context2dWithDims);
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
    default:
      console.log(`Ignoring command ${cmd}`);
  }
}

function clear({ctx, w, h}){
  ctx.clearRect(0, 0, w, h);
  click_targets = [];
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

async function loadImage(url, img) {
  return new Promise((resolve, reject) => {
    img.onload = () => resolve(img);
    img.onerror = reject;
    img.src = url;
  });
}

async function image({ctx}, command){
  const {src, x, y, w_scale: widthScale, h_scale: heightScale, name} = command;
  const img = document.createElement('img');
  console.log(`src: ${src}`);
  console.time('load image');

  await loadImage(src, img);

  console.timeEnd('load image');
  console.log(`img.src: ${img.src}`);
  console.dir(img);
  console.log(`image complete? ${img.complete}`);
  console.log('Finished loading image');
  const imageHeight = img.height * heightScale;
  const imageWidth = img.width * widthScale;
  const imageSourceX = 0;
  const imageSourceY = 0;
  ctx.drawImage(img, imageSourceX, imageSourceY, img.width, img.height, x, y, imageWidth, imageHeight);
  console.group();
  console.log(`imageHeight: ${imageHeight}`);
  console.log(`imageWidth: ${imageWidth}`);
  console.log(`y: ${y}`);
  console.log(`x: ${x}`);
  console.log(`img.width: ${img.width}`);
  console.log(`img.height: ${img.height}`);
  console.log(`imageSourceY: ${imageSourceY}`);
  console.log(`imageSourceX: ${imageSourceX}`);
  console.groupEnd();
  add_click_target({...command, type: 'square'});
}

function add_click_target(shape){
  click_targets.unshift(shape);
}

function get_click_target(event){
  const temp_click_targets = click_targets.slice();
  const {offsetX: x, offsetY: y} = event;
  const is_click_target_ = (ct) => is_click_target(ct, {x, y});
  const maybe_click_target = temp_click_targets.filter(is_click_target_)[0];
  if(maybe_click_target){
    document.querySelector(`#animator_controls_button_${maybe_click_target.name}`).click();
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
