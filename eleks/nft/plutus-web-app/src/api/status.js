import { wait } from '../helpers/utils';

const fetchUserPublicKey = async (wallet) =>
  await fetch(
    `http://localhost:8080/api/new/contract/instance/${wallet.id}/endpoint/userPubKeyHash`,
    {
      method: 'POST',
      headers: {
        'Content-type': 'application/json',
      },
      body: JSON.stringify([]),
    }
  );

const checkStatus = async (response, wallet, tag) => {
  const responseJSON = await response.json();
  const responseContent = responseJSON.cicCurrentState.observableState.Right;
  const error = responseJSON.cicCurrentState.observableState.Left;

  if (!responseContent) {
    await fetchUserPublicKey(wallet);
    return { error };
  } else if (responseContent.tag !== tag) {
    await wait(1000);
    return await fetchStatus(wallet, tag);
  } else {
    await fetchUserPublicKey(wallet);
    return responseContent;
  }
};

export async function fetchStatus(wallet, tag) {
  const response = await fetch(
    `http://localhost:8080/api/new/contract/instance/${wallet.id}/status`,
    {
      method: 'GET',
      headers: {
        'Content-type': 'application/json',
      },
    }
  );

  if (response.status === 200) {
    return checkStatus(response, wallet, tag);
  } else {
    return {
      error: 'Unable to fetch status',
    };
  }
}
