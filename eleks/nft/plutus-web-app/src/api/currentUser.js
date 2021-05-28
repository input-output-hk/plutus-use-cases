import { fetchStatus } from './status';
import { wait } from '../helpers/utils';

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
    await wait(1000);
    return await fetchStatus(wallet);
  } else {
    return {
      error: 'Unable to fetch public key',
    };
  }
}
