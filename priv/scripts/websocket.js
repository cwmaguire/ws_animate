// Create WebSocket connection.
const socket = new WebSocket("ws://localhost:8081/ws");
var channel;

// Connection opened
socket.addEventListener("open", (event) => {
  socket.send("Hello Server!");
  socket.send("channel start");
  socket.send("channel sub draw");
  socket.send("animator add animator1 a");
});

// Listen for messages
socket.addEventListener("message", (event) => {
  //console.log(`Event: ${event.data}`);
  obj = JSON.parse(event.data);
  if(obj.type == "draw"){
    draw(obj);
  }else if(obj.type == "info" && Object.hasOwn(obj, "channel_name")){
    channel = obj.channel_name;
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

function ctx(){
  const canvas = document.getElementById("canvas1");
  const ctx = canvas.getContext("2d");
  return {ctx: ctx, w: canvas.width, h: canvas.height};
}

function animationControls(){
  openNewWindow("animation_controls");
}

function openNewWindow(name) {
  const top = window.screenTop;
  const left = window.screenLeft;
  const url = `http://localhost:8081/html/${name}.html`;
  const qs = new URLSearchParams({channel: channel}).toString();
  window.open(`${url}?${qs}`, "_blank", `top=${top - 50},left=${left + 50},width=600,height=400,status=1,toolbar=1`);
}
