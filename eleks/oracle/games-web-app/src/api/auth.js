const MUTUAL_BET_URL = process.env.REACT_APP_MUTUAL_BET_URL;

export async function login(id) {
  const response = await fetch(`${MUTUAL_BET_URL}/wallet/${id}`, {
    method: 'GET',
    headers: {
      'Content-type': 'application/json',
    },
  });

  if (response.status === 200) {
    return response.json();
  } else {
    return {
      error: 'Unable to fetch wallet key',
    };
  }
}
