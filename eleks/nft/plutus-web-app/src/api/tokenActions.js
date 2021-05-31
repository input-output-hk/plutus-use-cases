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
    return await fetchStatus(wallet, "Selling");
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
      body: JSON.stringify(data),
    }
  );

  if (response.status === 200) {
    return await fetchStatus(wallet, "Buyed");
  } else {
    return {
      error: 'Unable to buy token',
    };
  }
}
