import { fetchStatus } from './status';

export async function fetchUserPublicKey(wallet) {
  const response = await fetch(
    `http://localhost:8080/api/new/contract/instance/${wallet.id}/endpoint/userPubKeyHash`,
    {
      method: 'POST',
      headers: {
        'Content-type': 'application/json',
      },
      body: JSON.stringify([]),
    }
  );

  if (response.status === 200) {
    return await fetchStatus(wallet, "UserPubKeyHash");
  } else {
    return {
      error: 'Unable to fetch public key',
    };
  }
}
