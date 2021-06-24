import { fetchStatus } from './status';

export async function fetchSellToken(wallet, data) {
  const response = await fetch(
    `http://localhost:8080/api/new/contract/instance/${wallet.id}/endpoint/sell`,
    {
      method: 'POST',
      headers: {
        'Content-type': 'application/json',
      },
      body: JSON.stringify(data),
    }
  );

  if (response.status === 200) {
    return await fetchStatus(wallet, 'Selling');
  } else {
    return {
      error: 'Unable to sell token',
    };
  }
}

export async function fetchBuyToken(wallet, data) {
  const response = await fetch(
    `http://localhost:8080/api/new/contract/instance/${wallet.id}/endpoint/buy`,
    {
      method: 'POST',
      headers: {
        'Content-type': 'application/json',
      },
      body: JSON.stringify({ bpTokenName: data.id }),
    }
  );

  if (response.status === 200) {
    return await fetchStatus(wallet, 'Buyed');
  } else {
    return {
      error: 'Unable to buy token',
    };
  }
}

export async function fetchCancelSellToken(wallet, data) {
  const response = await fetch(
    `http://localhost:8080/api/new/contract/instance/${wallet.id}/endpoint/cancelSell`,
    {
      method: 'POST',
      headers: {
        'Content-type': 'application/json',
      },
      body: JSON.stringify({ cspTokenName: data.id }),
    }
  );

  if (response.status === 200) {
    return await fetchStatus(wallet, 'CancelSelling');
  } else {
    return {
      error: 'Unable to cancel sell token',
    };
  }
}

export async function fetchTransferToken(wallet, data) {
  const response = await fetch(
    `http://localhost:8080/api/new/contract/instance/${wallet.id}/endpoint/transfer`,
    {
      method: 'POST',
      headers: {
        'Content-type': 'application/json',
      },
      body: JSON.stringify({
        tpTokenName: data.tokenId,
        tpReceiverWallet: data.walletNumber,
      }),
    }
  );

  if (response.status === 200) {
    return await fetchStatus(wallet, 'Transfered');
  } else {
    return {
      error: 'Unable to transfer token',
    };
  }
}
