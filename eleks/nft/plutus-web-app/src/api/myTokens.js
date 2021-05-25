import { NFTStorage } from 'nft.storage';
import { IPFS_API_TOKEN } from '../helpers/constants';

const wait = (ms) => new Promise((r) => setTimeout(() => r(), ms));

async function fetchStatus(wallet) {
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
    return response.json();
  } else {
    return {
      error: 'Unable to fetch status',
    };
  }
}

export async function fetchAddToken(wallet, data) {
  const clientIPFS = new NFTStorage({ token: IPFS_API_TOKEN });
  const cpFile = await clientIPFS.storeBlob(data.cpFile);

  const response = await fetch(
    `http://localhost:8080/api/new/contract/instance/${wallet.id}/endpoint/create`,
    {
      method: 'POST',
      headers: {
        'Content-type': 'application/json',
        'Access-Control-Allow-Origin': '*',
      },
      body: JSON.stringify({ ...data, cpFile }),
    }
  );

  if (response.status === 200) {
    await wait(10000);
    return {};
  } else {
    return {
      error: 'Unable to add token',
    };
  }
}

export async function fetchMyTokens(wallet) {
  const response = await fetch(
    `http://localhost:8080/api/new/contract/instance/${wallet.id}/endpoint/userNftTokens`,
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
      error: 'Unable to fetch tokens',
    };
  }
}
