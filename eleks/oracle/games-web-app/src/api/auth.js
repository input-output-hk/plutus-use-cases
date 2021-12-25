const WALLET_URL = process.env.REACT_APP_WALLET_URL;

export async function login(id) {
  const response = await fetch(`${WALLET_URL}/${id}/own-payment-public-key`, {
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
