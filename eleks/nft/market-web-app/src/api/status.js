import { wait } from '../helpers/utils';
import { errorMap } from '../helpers/errorMap.js';
const API_URL = process.env.REACT_APP_API_URL;

const fetchUserPublicKey = async (wallet) =>
  await fetch(`${API_URL}/${wallet.id}/endpoint/userPubKeyHash`, {
    method: 'POST',
    headers: {
      'Content-type': 'application/json',
    },
    body: JSON.stringify([]),
  });

const mapError = (error) => {
  const parsedError = error.replace('(', '').split(' ');
  const errorKey = parsedError[1];

  if (parsedError[0] === 'WalletError') {
    if (parsedError[1] === 'ValidationError') {
      var matched = error.match('(?<=\\[").+?(?="\\])');
      return { error: matched[0] || parsedError };
    } else return { error: errorMap[errorKey] || parsedError };
  } else {
    return { error };
  }
};

const checkStatus = async (response, wallet, tag) => {
  const responseJSON = await response.json();
  const responseContent = responseJSON.cicCurrentState.observableState.Right;
  const error = responseJSON.cicCurrentState.observableState.Left;

  if (!responseContent) {
    await fetchUserPublicKey(wallet);
    return mapError(error);
  } else if (responseContent.tag !== tag) {
    await wait(1000);
    return await fetchStatus(wallet, tag);
  } else {
    await fetchUserPublicKey(wallet);
    return responseContent;
  }
};

export async function fetchStatus(wallet, tag) {
  const response = await fetch(`${API_URL}/${wallet.id}/status`, {
    method: 'GET',
    headers: {
      'Content-type': 'application/json',
    },
  });

  if (response.status === 200) {
    return checkStatus(response, wallet, tag);
  } else {
    return {
      error: 'Unable to fetch status',
    };
  }
}
