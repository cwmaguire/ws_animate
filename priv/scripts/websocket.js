'use strict';
var context2dWithDims;
var channel;
var canvas;
var loadSaveDiv;
var animatorButtonDiv;
var animatorControlsButtonDiv;
var animatorIndex = 1;
var receive_buffer = [];
var draw_buffer = [];
var click_targets = [];
const saveFileDataListId = 'save_file_datalist';
const loadFileSelectId = 'load_file_select';

create_canvas_and_controls();

const socket = new WebSocket('ws://localhost:8081/ws');
socket.addEventListener('open', websocket_open);
socket.addEventListener('message', websocket_message);
socket.addEventListener('error', websocket_error);
socket.addEventListener('close', websocket_close);

function websocket_open(event){
  socket.send('channel start');
  socket.send('channel sub draw');
  socket.send('channel sub info');
  socket.send('animator list');
  requestAnimationFrame(animation_frame);
}

function websocket_message(event){
  let obj = JSON.parse(event.data);
  switch(obj.type){
    case 'draw':
      buffer_command(obj);
      break;
    case 'info':
      info(obj);
      break;
    case 'log':
      console.log(obj.log);
      break;
  }
}

function websocket_error(event){
  console.log('Websocket error: ', event);
}

function websocket_close(event){
  console.log('Websocket close. Code: ', event.code, '. Reason: \'', event.reason, '\'. Clean? ', event.wasClean);
}

function load_animations(){
  const filename = document.querySelector('#load_file_select').value;
  socket.send(`channel load ${filename}`);
}

function save_animations(){
  const saveFileText = document.querySelector('#save_file');
  const filename = saveFileText.value;
  socket.send(`channel save ${filename}`);
  saveFileText.value = '';
}

function info(obj){
  if('channel_name' in obj){
    channel = obj.channel_name;
  }else if('animators' in obj){
    animator_buttons(obj.animators);
  }else if('animator_name' in obj){
    animation_controls_button(obj.animator_name);
  }else if('filenames' in obj){
    console.log('Received filenames ...');
    console.dir(obj);
    set_filenames(obj.filenames);
  }
}

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
}

function open_new_window(name, qsParams, button) {
  const top = window.screenTop;
  const left = window.screenLeft;
  const url = `http://localhost:8081/html/${name}.html`;
  const qs = new URLSearchParams(qsParams).toString();
  console.log(`Query string for new window: ${qs}`);
  const newWindow = window.open(`${url}?${qs}`, '_blank', `top=${top - 50},left=${left + 50},width=600,height=400,status=1,toolbar=1`);
  newWindow.addEventListener('load', (event) => {
  });
  button.old_window = newWindow;
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

function create_canvas_and_controls(){

  loadSaveDiv = div('load_save_div');
  load_save_controls(loadSaveDiv);

  animatorButtonDiv = div('animator_controls_div');
  animatorControlsButtonDiv = div('animator_controls_button_div');


  create_canvas();
  document.body.appendChild(loadSaveDiv);
  document.body.appendChild(animatorButtonDiv);
  document.body.appendChild(animatorControlsButtonDiv);
  document.body.appendChild(canvas);
}

function create_canvas(){
  canvas = document.createElement('canvas');
  canvas.id = 'canvas1';
  canvas.width = 800;
  canvas.height = 700;

  const ctx = canvas.getContext('2d');
  context2dWithDims = {ctx: ctx,
                       w: canvas.width,
                       h: canvas.height};
  canvas.addEventListener('click', get_click_target);
}

function div(id){
  const div = document.createElement('div');
  div.id = id;
  document.body.appendChild(div);
  return div;
}

function load_save_controls(loadSaveDiv){
  button('load', load_animations);
  select_list(loadFileSelectId);
  br();
  button('save', save_animations);
  text_with_datalist('save_file', saveFileDataListId, (e) => {});
}

function br(){
  document.body.appendChild(document.createElement('br'));
}

function button(id, clickEventHandler){
  const button = document.createElement('input');
  button.type = 'button';
  button.id = id;
  button.value = id;
  button.addEventListener('click', clickEventHandler);
  document.body.appendChild(button);
}

function text_with_datalist(textId, datalistId, handler){
  const dl = document.createElement('datalist');
  dl.id = datalistId;
  document.body.appendChild(dl);

  const s = document.createElement('input');
  s.type = 'text'
  s.setAttribute('list', datalistId);
  s.id = textId;
  s.name = textId;
  document.body.appendChild(s);
}

function set_filenames(filenames){

  const dl = document.querySelector('#' + saveFileDataListId);
  const select = document.querySelector('#' + loadFileSelectId);

  while(dl.firstChild){
    dl.removeChild(dl.firstChild)
  }
  while(select.firstChild){
    select.removeChild(select.firstChild)
  }

  filenames.forEach((filename) => {
    dl.appendChild(option(filename));
    select.appendChild(option(filename));
  });
}

function option(text){
  const o = document.createElement('option');
  o.value = text;
  o.textContent = text;
  return o;
}

function select_list(id){
  const s = document.createElement('select');
  s.id = id;
  s.name = name;

  document.body.appendChild(s);
}

function animator_buttons(animators){
  animatorButtonDiv.childNodes.forEach(({child}) => child.remove());
  animators.forEach((animator) => {animator_button(animator)});
}

function animator_button({name, short_name}){
  const button = document.createElement('input');
  button.id = `animator_button_${name}`;
  button.type = 'button';
  button.value = name;
  button.addEventListener('click',
    (event) => {
      socket.send(`animator add ${name} ${short_name}_${animatorIndex.toString()}`);
      animatorIndex += 1;
    });
  animatorButtonDiv.appendChild(button);
}

function animation_controls_button(name){
  const button = document.createElement('input');
  button.id = `animator_controls_button_${name}`;
  button.type = 'button';
  button.value = name;
  const qsParams = {channel, animator: name};
  button.addEventListener('click',
    (event) => {
      if('old_window' in button && button.old_window.closed){
        delete button.old_window;
        open_new_window('animation_controls', qsParams, button);
      }else if('old_window' in button){
        button.old_window.focus();
      }else{
        open_new_window('animation_controls', qsParams, button);
      }
    });
  animatorControlsButtonDiv.appendChild(button);
}
