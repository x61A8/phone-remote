// WebSockets
function getWS () {
    const url = new URL(window.location.href);
    const address = url.hostname;
    const wsPort = url.searchParams.get("ws");
    const wsUrl = "ws://" + address + ":" + wsPort + "/";
    return new WebSocket(wsUrl);
}
const socket = getWS();
let player = 1;
