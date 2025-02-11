// Create WebSocket connection.
const socket = new WebSocket("ws://localhost:8081/ws");
var channel;
var animatorIndex = 1;

// Connection opened
socket.addEventListener("open", (event) => {
  socket.send("channel start");
  socket.send("channel sub draw");
  socket.send("animator list");
  //socket.send("channel sub log");
  //socket.send("animator add animator1 a");
});

// Listen for messages
socket.addEventListener("message", (event) => {
  console.log(`Event: ${event.data}`);
  obj = JSON.parse(event.data);
  if(obj.type == "draw"){
    draw(obj);
  }else if(obj.type == "info" && Object.hasOwn(obj, "channel_name")){
    channel = obj.channel_name;
  }else if(obj.type == "info" && Object.hasOwn(obj, "animators")){
    console.log(`Found animators ${obj.animators.join(',')}`)
    animator_buttons(obj.animators);
  }else if(obj.type == "info" && Object.hasOwn(obj, "animator_name")){
    animator_server_button(obj.animator_name);
  }else if(obj.type == "log"){
    console.log(obj.log);
  }
});

socket.addEventListener("error", (event) => {
  console.log("Websocket error: ", event);
});

socket.addEventListener("close", (event) => {
  console.log("Websocket close. Code: ", event.code, ". Reason: \"", event.reason, "\". Clean? ", event.wasClean);
});

function notNullOrUndefined(val){
  return val != null; // checks for null and undefined since they both equate to null
}

function animator_buttons(names){
  document.getElementById("animator_button_div").childNodes.forEach(({child}) => child.remove());
  names.forEach((name) => {console.log(name); animator_button(name)});
}

function animator_button(name){
  const button = document.createElement("input");
  button.id = `animator_button_${name}`;
  button.type = "button";
  button.value = name;
  button.addEventListener("click",
    (event) => {
      socket.send(`animator add ${name} ${animatorIndex.toString()}`);
      animatorIndex += 1;
    });
  document.getElementById("animator_button_div").appendChild(button);
}

function animator_server_button(name){
  const button = document.createElement("input");
  button.id = `animator_controls_button_${name}`;
  button.type = "button";
  button.value = name;
  const qsParams = {channel: channel, animator: name};
  button.addEventListener("click", (event) => { openNewWindow("animation_controls", qsParams) })
  document.getElementById("animator_controls_button_div").appendChild(button);
}

function draw(Command){
  const {cmd} = Command;
  c = ctx();
  switch(cmd){
    case "clear":
      clear(c);
      break;
    case "square":
      square(c, Command);
      break;
    case "circle":
      circle(c, Command);
      break;
    default:
      console.log(`Ignoring command ${cmd}`);
  }
}

function clear({ctx, w, h}){
  ctx.clearRect(0, 0, w, h);
}

function square({ctx}, {x, y, w, h, style}){
  //console.log(`square: x: ${x}, y: ${y}, w: ${w}, h: ${h}, style: ${style}`);
  ctx.strokeSyle = style;
  ctx.strokeRect(x, y, w, h);
}

function circle({ctx}, {x, y, r, style}){
  //console.log(`square: x: ${x}, y: ${y}, w: ${w}, h: ${h}, style: ${style}`);
  ctx.strokeSyle = style;
  ctx.moveTo(x, y);
  ctx.beginPath();
  ctx.ellipse(x, y, r, r, 0, 0, 2 * Math.PI);
  ctx.closePath();
  ctx.stroke();
}

function ctx(){
  const canvas = document.getElementById("canvas1");
  const ctx = canvas.getContext("2d");
  return {ctx: ctx, w: canvas.width, h: canvas.height};
}

function animationControls(){
  openNewWindow("animation_controls");
}

function openNewWindow(name, qsParams) {
  const top = window.screenTop;
  const left = window.screenLeft;
  const url = `http://localhost:8081/html/${name}.html`;
  const qs = new URLSearchParams(qsParams).toString();
  console.log(`Query string for new window: ${qs}`);
  const newWindow = window.open(`${url}?${qs}`, "_blank", `top=${top - 50},left=${left + 50},width=600,height=400,status=1,toolbar=1`);
  newWindow.addEventListener("load", (event) => {
    socket.addEventListener("close", (event) => { newWindow.close(); });
  });
}
