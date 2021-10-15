export async function fetchGames() {
  const response = await fetch(`http://localhost:8081/games`, {
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
  const response = await fetch(`http://localhost:8081/games/${id}`, {
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
