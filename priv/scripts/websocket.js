'use strict';
var context2dWithDims;
var channel;
var canvas;
var channelControlsDiv;
var loadSaveDiv;
var animatorButtonDiv;
var animatorControlsButtonDiv;
var animatorControlsDiv;
var animatorIndex = 1;
var receive_buffer = [];
var draw_buffer = [];
var click_targets = [];
var shouldUseControlsPopup = false;
var shouldUseControlsPanel = true;
const channelSelectId = 'channel_select';
const saveFileDataListId = 'save_file_datalist';
const loadFileSelectId = 'load_file_select';
const controlPanels = [];

create_canvas_and_controls();
const socket = create_socket();

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
  }else if('channel' in obj){
    console.log('Received channel ...');
    console.dir(obj);
    if(obj.channel != channel){
      add_channel(obj.channel);
    }else{
      console.log(`Not adding channel ${obj.channel} because we're in it (${channel})`);
    }
  }else if(obj?.info == 'clear_channels'){
    console.log('Received clear channels');
    clear_channels();
  }else if(obj?.info == 'clear_animator_names'){
    console.log('Received clear animator names');
    clear_animator_names();
  }else if(!('avg_frame_time' in obj)){
    console.log('Server: unrecognized message ...');
    console.dir(obj);
  }
}

function open_new_window(name, qsParams, button) {
  const top = window.screenTop;
  const left = window.screenLeft;
  const url = `http://localhost:8081/html/${name}.html`;
  const qs = new URLSearchParams(qsParams).toString();
  console.log(`Query string for new window: ${qs}`);
  const newWindow = window.open(`${url}?${qs}`,
    '_blank',
    `top=${top - 50},left=${left + 50},width=800,height=700,status=1,toolbar=1`);
  // XXX Not sure what this was for
  //newWindow.addEventListener('load', (event) => { });
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

function create_canvas_and_controls(){
  channel_controls_div();
  load_save_controls_div();
  page_controls_div();
  animator_button_div();
  animator_controls_button_div();
  add_canvas();
  animator_controls_fifo_panel();
}

function channel_controls_div(){
  add_populate('channel_div', channel_controls);
}

function channel_controls(){
  return [button('join', switch_channel),
          select_list(channelSelectId)];
}

function page_controls_div(){
  add_populate('page_controls_div', page_controls);
}

function page_controls(){
  const controls1 = checkbox('should_use_controls_popup', toggle_controls_popup, 'controls popup?', false);
  const controls2 = checkbox('should_use_controls_panel', toggle_controls_panel, 'controls panel?', true);
  return controls1.concat(controls2);
}

function load_save_controls_div(){
  add_populate('load_save_div', load_save_controls);
}

function animator_button_div(){
  animatorButtonDiv = div('animator_button_div');
  document.body.appendChild(animatorButtonDiv);
}

function animator_controls_button_div(){
  animatorControlsButtonDiv = div('animator_controls_button_div');
  document.body.appendChild(animatorControlsButtonDiv);
}

function animator_controls_fifo_panel(){
  animatorControlsDiv = div('animator_controls_div')
  animatorControlsDiv.style.left = '700px';
  animatorControlsDiv.style.top = '200px';
  animatorControlsDiv.style.width = '310px';
  animatorControlsDiv.style.position = 'absolute';
  animatorControlsDiv.style.border = '1px solid red';
  document.body.appendChild(animatorControlsDiv);
}

function add_canvas(){
  canvas = document.createElement('canvas');
  canvas.id = 'canvas1';
  canvas.width = 800;
  canvas.height = 700;
  const shouldAddClickTargeting = true;
  setup_canvas(canvas, shouldAddClickTargeting);
  document.body.appendChild(canvas);
}

function div(id){
  const div = document.createElement('div');
  div.id = id;
  return div;
}

function add_populate(Id, Fun){
  const div_ = div(Id);
  const elements = Fun();
  elements.forEach((e) => div_.appendChild(e));
  document.body.appendChild(div_);
  return div_;
}

function load_save_controls(loadSaveDiv){
  const controls =
    [button('load', load_animations),
     select_list(loadFileSelectId),
     br(),
     button('save', save_animations)];
  const datalistControls = text_with_datalist('save_file', saveFileDataListId);
  return controls.concat(datalistControls);
}

function br(){
  return document.createElement('br');
}

function button(id, clickEventHandler){
  const button = document.createElement('input');
  button.type = 'button';
  button.id = id;
  button.value = id;
  button.addEventListener('click', clickEventHandler);
  return button;
}

function checkbox(id, clickEventHandler, labelText, isChecked){
  const checkbox = document.createElement('input');
  checkbox.type = 'checkbox';
  checkbox.id = id;
  checkbox.value = id;
  checkbox.checked = isChecked;
  checkbox.addEventListener('click', clickEventHandler);
  const label = document.createElement('label');
  label.textContent = labelText;
  label.htmlFor = id;
  return [label, checkbox];
}

function text_with_datalist(textId, datalistId){
  const dl = document.createElement('datalist');
  dl.id = datalistId;

  const s = document.createElement('input');
  s.type = 'text'
  s.setAttribute('list', datalistId);
  s.id = textId;
  s.name = textId;

  return [dl, s];
}

function set_filenames(filenames){
  const dl = document.querySelector('#' + saveFileDataListId);
  const select = document.querySelector('#' + loadFileSelectId);

  clear_children(dl);
  clear_children(select);

  filenames.forEach((filename) => {
    dl.appendChild(option(filename));
    select.appendChild(option(filename));
  });
}

function add_channel(channel){
  const select = document.querySelector('#' + channelSelectId);
  select.appendChild(option(channel));
}

function clear_channels(){
  clear_children(document.querySelector(`#${channelSelectId}`));
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
  return s;
}

function animator_buttons(animators){
  clear_children(animatorButtonDiv);
  animators.forEach((a) => {animator_button(a)});
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
  const eventListener = anim_ctrls_click_fun(button, channel, name);
  button.addEventListener('click', eventListener),
  animatorControlsButtonDiv.appendChild(button);
}

function anim_ctrls_click_fun(button, channel, name){
  const qsParams = {channel, animator: name};
  const fun =
    function(event){
      if(shouldUseControlsPopup && button.old_window?.closed){
        delete button.old_window;
        open_new_window('animation_controls', qsParams, button);
      }else if('old_window' in button){
        button.old_window.focus();
      }else if(shouldUseControlsPopup){
        open_new_window('animation_controls', qsParams, button);
      }

      if(shouldUseControlsPanel && !button.old_div){
        add_animation_controls_div(channel, name);
      }
    };
  return fun;
}

function add_animation_controls_div(channel, name){
  while(controlPanels.length > 1){
    controlPanels.pop().remove();
  }
  const panel = animator_controls_panel(channel, name);
  controlPanels.unshift(panel);
  animatorControlsDiv.appendChild(panel);
}

function animator_controls_panel(channel, name){
  const div = document.createElement('div');
  div.style.width = '300px';
  div.style.border = '1px solid blue';
  animation_controls_start(channel, name, div, false);
  return div;
}

function clear_children(element){
  while(element.firstChild){
    element.removeChild(element.firstChild);
  }
}

function switch_channel(){
  socket.send('channel leave');
  const newChannel = document.querySelector(`#${channelSelectId}`).value;
  socket.send(`channel join ${newChannel}`);
  socket.send(`channel sub draw`);
  socket.send('channel sub info');
  socket.send('animator list');
}

function toggle_controls_popup(e){
  shouldUseControlsPopup = e.target.checked;
}

function toggle_controls_panel(e){
  shouldUseControlsPanel = e.target.checked;
}

function clear_animator_names(){
  clear_children(animatorControlsButtonDiv);
}
