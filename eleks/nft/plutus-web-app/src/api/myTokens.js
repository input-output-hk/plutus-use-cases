import { NFTStorage } from 'nft.storage';
import { IPFS_API_TOKEN } from '../helpers/constants';

import { fetchStatus } from './status';
import { wait } from '../helpers/utils';

export async function fetchAddToken(wallet, data) {
  const clientIPFS = new NFTStorage({ token: IPFS_API_TOKEN });
  const cpFile = await clientIPFS.storeBlob(data.cpFile);

  const response = await fetch(
    `http://localhost:8080/api/new/contract/instance/${wallet.id}/endpoint/create`,
    {
      method: 'POST',
      headers: {
        'Content-type': 'application/json'
      },
      body: JSON.stringify({ ...data, cpFile }),
    }
  );

  if (response.status === 200) {
    await wait(1000);
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
