'use strict';

var channel;
var canvas;
var channelControlsDiv;
var loadSaveDiv;
var animatorButtonDiv;
var animatorControlsButtonDiv;
var animatorControlsDiv;
var animatorIndex = 1;
var shouldUseControlsPopup = false;
var shouldUseControlsPanel = true;
var video;
var imageWorker;

const channelSelectId = 'channel_select';
const saveFileDataListId = 'save_file_datalist';
const loadFileSelectId = 'load_file_select';
const controlPanels = [];
const loadedImages = new Map;
const PATH_TO_IMAGES = 'images/';
const deviceId = 'cde819aad3a5f7da8759721271d1d3deaf3dbdce8666ecfb5f4180f93f5e0d00';

const socketWorker = new Worker('scripts/bg_socket.js');
socketWorker.addEventListener('message', dispatch);

create_canvas_and_controls();

function load_animations(){
  const filename = document.querySelector('#load_file_select').value;
  const command =
    {target: 'server',
      command: `load animations ${filename}`};
  socketWorker.postMessage(command);
}

function save_animations(){
  const saveFileText = document.querySelector('#save_file');
  const filename = saveFileText.value;
  const command =
    {target: 'server',
      command: `load animations ${filename}`};
  socketWorker.postMessage(command);
  saveFileText.value = '';
}

function dispatch({data}){
  if('channel' in data){
    imageWorker = new Worker('scripts/bg_send_image.js');
    const command =
      {target: 'send_image',
       command: 'channel',
       channel: data.channel};
    imageWorker.postMessage(command);
    channel = data.channel
  }else if('animators' in data){
    animator_buttons(data.animators);
  }else if('animator_name' in data){
    animation_controls_button(data.animator_name);
  }else if('filenames' in data){
    console.log('Received filenames ...');
    console.dir(data);
    set_filenames(data.filenames);
  }else if('channel' in data){    // why 'channel' again? (see first if branch)
    if(data.channel != channel){
      add_channel(data.channel);
    }else{
      console.log(`Not adding channel ${data.channel} because we're in it (${channel})`);
    }
  }else if('frame_millis' in data){
    document.querySelector('#channel_frame_millis').value = data.frame_millis;
  }else if(data?.info == 'clear_channels'){
    console.log('Received clear channels');
    clear_channels();
  }else if(data?.info == 'clear_animator_names'){
    console.log('Received clear animator names');
    clear_animator_names();
  }else if(data?.info == 'send_image'){
    console.log(`Image requested: ${data.deviceId}, ${data.animatorName}`);
    send_image(data.deviceId, data.animatorName);
  }else if(!('avg_frame_time' in data)){
    console.log('Server: unrecognized message ...');
    console.dir(data);
  }else if(data?.command == 'wait video'){
    wait_video(data.deviceId);
  }else if(data?.command == 'wait image'){
    wait_image(data.src);
  }else{
    console.log('main unexpected command ...');
    console.dir(data);
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
  const switchControls = [button('join', switch_channel),
                          select_list(channelSelectId),
                          br()];
  const frameMillisControls = number('channel_frame_millis', set_frame_millis, 'frame_millis');
  return switchControls.concat(frameMillisControls);
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
  canvas.addEventListener('click', get_click_target);
  const shouldAddClickTargeting = true;
  const offscreenCanvas = canvas.transferControlToOffscreen();
  //setup_canvas(canvas, shouldAddClickTargeting);
  const command =
    {target: 'draw',
     command: 'transfer canvas',
     canvas: offscreenCanvas,
     shouldAddClickTargeting,
     transfer: [offscreenCanvas]};
  socketWorker.postMessage(command, [offscreenCanvas]);
  document.body.appendChild(canvas);
}

function get_click_target(event){
  const {offsetX: x, offsetY: y} = event;
  const command = {target: 'draw',
                   command: 'get click target',
                   event: {x, y}};
  socketWorker.postMessage(command);
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

function number(id, changeEventHandler, labelText){
  const input = document.createElement('input');
  input.type = 'number';
  input.size = '5';
  input.id = id;
  input.addEventListener('change', changeEventHandler);
  const label = document.createElement('label');
  label.textContent = labelText,
  label.htmlFor = id;
  return [label, input];
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
      const command =
        {target: 'server',
         command: `animator add ${name} ${short_name}_${animatorIndex.toString()}`};
      socketWorker.postMessage(command);
      animatorIndex += 1;
    });
  animatorButtonDiv.appendChild(button);
}

function animation_controls_button(animatorName){
  const button = document.createElement('input');
  button.type = 'button';
  button.id = `animator_controls_button_${animatorName}`;
  button.value = animatorName;
  const eventListener = anim_ctrls_click_fun(button, channel, animatorName);
  button.addEventListener('click', eventListener),
  animatorControlsButtonDiv.appendChild(button);
}

function anim_ctrls_click_fun(button, channel, animatorName){
  const qsParams = {channel, animator: animatorName};
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

      if(shouldUseControlsPanel && !(document.querySelector(`#${animatorName}_control_panel`))){
        const div = add_animation_controls_div(channel, animatorName, button);
      }
    };
  return fun;
}

function add_animation_controls_div(channel, name, button){
  while(controlPanels.length > 1){
    const panel = controlPanels.pop();
    panel.remove();
  }
  const panel = animator_controls_panel(channel, name);
  controlPanels.unshift(panel);
  animatorControlsDiv.appendChild(panel);
  return panel;
}

function animator_controls_panel(channel, animatorName){
  const div = document.createElement('div');
  div.style.width = '300px';
  div.style.border = '1px solid blue';
  div.id = `${animatorName}_control_panel`;
  animation_controls_start(channel, animatorName, div, false);
  return div;
}

function clear_children(element){
  while(element.firstChild){
    element.removeChild(element.firstChild);
  }
}

function switch_channel(){
  const newChannel = document.querySelector(`#${channelSelectId}`).value;
  const command =
    {target: 'server',
     command: 'switch channel',
     channel: newChannel};
  socketWorker.postMessage(command);
}

function set_frame_millis(e){
  const frameMillis = document.querySelector('#channel_frame_millis').value;
  const command =
    {target: 'server',
     command: `channel set frame_millis ${frameMillis}`};
  socketWorker.postMessage(command);
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

async function send_image(deviceId, animatorName){
    await wait_video(deviceId);
    video.requestVideoFrameCallback(() => send_image_(animatorName));
}

function send_image_(animatorName){
  const videoFrame = new VideoFrame(video);
  const command =
    {target: 'send_image',
     command: 'send_image',
     videoFrame,
     animatorName};
  imageWorker.postMessage(command);
}

// XXX assumes only one device ID is ever used
// TODO set device ID on video DOM object
async function wait_video(deviceId){
  if(!video){
    video = document.createElement('video');
    //video.style.width = '40px';
    //video.style.height = '30px';
    document.body.appendChild(video);
    await start_webcam(deviceId);
  }else{
     notify_video(deviceId);
  }
}

async function start_webcam(deviceId){
  const videoConstraint = {video: {deviceId: deviceId}};
  return navigator.mediaDevices.getUserMedia(videoConstraint)
    .then(play_video)
    .then(() => notify_video(deviceId));
}

function play_video(stream){
  video.srcObject = stream;
  return video.play();
}

function notify_video(){
  const command =
    {target: 'draw',
     command: 'video is ready',
     deviceId};
  socketWorker.postMessage(command);
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

// TODO send images back to draw worker
function load_image(img, url) {
  return new Promise((resolve, reject) => {
    // TODO since the caller throws away the img result, we could
    // probably just say:
    // img.onload = resolve
    // i.e. when the 'load' event fires, call resolve(...) to signal
    // the the promise has been resolved, and, since we're returning
    // a value, it's also been fulfilled.
    // (I believe a resolved Promise can trigger another Promise, in
    // which case the first promise is "resolved" but not "fulfilled")
    img.onload = () => resolve(img);
    img.onerror = reject;
    img.src = url;
  });
}
