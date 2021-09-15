import { fetchStatus } from './status';

export async function fetchStorefront(wallet) {
  const response = await fetch(
    `http://localhost:8080/api/contract/instance/${wallet.id}/endpoint/sellingTokens`,
    {
      method: 'POST',
      headers: {
        'Content-type': 'application/json',
      },
      body: JSON.stringify([]),
    }
  );

  if (response.status === 200) {
    return await fetchStatus(wallet, "SellingTokens");
  } else {
    return {
      error: 'Unable to fetch storefront tokens',
    };
  }
}
