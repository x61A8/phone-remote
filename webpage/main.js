// WebSockets
function getWS () {
    const url = new URL(window.location.href);
    const address = url.hostname;
    const wsPort = url.searchParams.get("ws");
    const wsUrl = "ws://" + address + ":" + wsPort + "/";
    return new WebSocket(wsUrl);
}
const socket = getWS();

// Player Switching
let player = 1;
const playerButton = document.getElementById("player");
function switchPlayer () {
    if (player === 1) {
	player = 2;
    }
    else {
	player = 1;
    }
    playerButton.textContent = `Player ${player}`;
}
playerButton.addEventListener('click', e => {
    switchPlayer();
});

