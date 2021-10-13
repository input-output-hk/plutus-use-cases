export async function fetchContracts(id) {
  const response = await fetch(
    `http://localhost:9080/api/contract/instances/wallet/${id}`,
    {
      method: 'GET',
      headers: {
        'Content-type': 'application/json',
      },
    }
  );

  if (response.status === 200) {
    return response.json();
  } else {
    return {
      error: 'Unable to fetch contracts',
    };
  }
}

export async function fetchGameBets(id) {
  const response = await fetch(
    `http://localhost:9080/api/contract/instance/${id}/status`,
    {
      method: 'GET',
      headers: {
        'Content-type': 'application/json',
      },
    }
  );

  if (response.status === 200) {
    return response.json();
  } else {
    return {
      error: 'Unable to fetch bets',
    };
  }
}

export async function makeBet(nbpWinnerId, nbpAmount, gameId) {
  const response = await fetch(
    `http://localhost:9080/api/contract/instance/${gameId}/endpoint/bet`,
    {
      method: 'POST',
      headers: {
        'Content-type': 'application/json',
      },
      body: JSON.stringify({ nbpWinnerId, nbpAmount: +nbpAmount }),
    }
  );

  if (response.status === 200) {
    return response.json();
  } else {
    return {
      error: 'Unable to make a bet',
    };
  }
}
