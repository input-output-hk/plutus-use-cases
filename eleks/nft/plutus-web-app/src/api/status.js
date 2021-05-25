export async function fetchStatus(wallet) {
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
