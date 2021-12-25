import { errorMap } from '../helpers/errorMap';
const BET_PUB_URL = process.env.REACT_APP_BET_PAB_URL;

export async function fetchContracts(id) {
  const response = await fetch(`${BET_PUB_URL}/instances/wallet/${id}`, {
    method: 'GET',
    headers: {
      'Content-type': 'application/json',
    },
  });

  if (response.status === 200) {
    return response.json();
  } else {
    return {
      error: 'Unable to fetch contracts',
    };
  }
}

export async function fetchGameBets(id) {
  const response = await fetch(`${BET_PUB_URL}/instance/${id}/status`, {
    method: 'GET',
    headers: {
      'Content-type': 'application/json',
    },
  });

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
    `${BET_PUB_URL}/instance/${gameId}/endpoint/bet`,
    {
      method: 'POST',
      headers: {
        'Content-type': 'application/json',
      },
      body: JSON.stringify({ nbpWinnerId, nbpAmount: +nbpAmount }),
    }
  );

  if (response.status === 200) {
    const statusResponse = await fetchGameBets(gameId);
    if (statusResponse.cicCurrentState.err) {
      return { error: errorMap(statusResponse, 'Unable to make a bet') };
    } else {
      return response.json();
    }
  } else {
    return {
      error: 'Unable to make a bet',
    };
  }
}

export async function cancelBet(nbpWinnerId, nbpAmount, gameId) {
  const response = await fetch(
    `${BET_PUB_URL}/instance/${gameId}/endpoint/cancelBet`,
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
      error: 'Unable to cancel a bet',
    };
  }
}
