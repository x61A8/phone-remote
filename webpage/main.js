// WebSockets
function getWS () {
    const url = new URL(window.location.href);
    const address = url.hostname;
    const wsPort = url.searchParams.get('ws');
    const wsUrl = 'ws://' + address + ':' + wsPort + '/';
    return new WebSocket(wsUrl);
}
const socket = getWS();

// Player Switching
let player = 1;
const playerButton = document.getElementById('player');
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

// Input Handling
function sendKey (key) {
    const message = key + player;
    socket.send(message);
}

const intervals = new Map();
function addButtonListeners (id, key) {
    const button = document.getElementById(id);
    button.addEventListener('pointerdown', e => {
	sendKey(key);
	intervals.set(key, setInterval(function(){sendKey(key);}, 50));
    });
    button.addEventListener('pointerup', e => {
	clearInterval(intervals.get(key));
    });
}

addButtonListeners('left', 'L');
addButtonListeners('up', 'U');
addButtonListeners('down', 'D');
addButtonListeners('right', 'R');
addButtonListeners('start', '*');
addButtonListeners('select', 'E');
addButtonListeners('b', 'B');
addButtonListeners('a', 'A');
