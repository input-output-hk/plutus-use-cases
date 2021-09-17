import { fetchStatus } from './status';
const API_URL = process.env.REACT_APP_API_URL;

export async function fetchUserPublicKey(wallet) {
  const response = await fetch(
    `${API_URL}/${wallet.id}/endpoint/userPubKeyHash`,
    {
      method: 'POST',
      headers: {
        'Content-type': 'application/json',
      },
      body: JSON.stringify([]),
    }
  );

  if (response.status === 200) {
    return await fetchStatus(wallet, 'UserPubKeyHash');
  } else {
    return {
      error: 'Unable to fetch public key',
    };
  }
}
