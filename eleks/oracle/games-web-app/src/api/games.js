const GAME_URL = process.env.REACT_APP_GAME_URL;

export async function fetchGames() {
  const response = await fetch(`${GAME_URL}/games`, {
    method: 'GET',
    headers: {
      'Content-type': 'application/json',
    },
  });

  if (response.status === 200) {
    return response.json();
  } else {
    return {
      error: 'Unable to fetch games',
    };
  }
}

export async function fetchGame(id) {
  const response = await fetch(`${GAME_URL}/games/${id}`, {
    method: 'GET',
    headers: {
      'Content-type': 'application/json',
    },
  });

  if (response.status === 200) {
    return response.json();
  } else {
    return {
      error: 'Unable to fetch game',
    };
  }
}
