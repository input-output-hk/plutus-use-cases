export async function login(id) {
  const response = await fetch(`http://localhost:8082/wallet/${id}`, {
    method: 'GET',
    headers: {
      'Content-type': 'application/json',
    },
  });

  if (response.status === 200) {
    return response.json();
  } else {
    return {
      error: 'Unable to fetch wallet key',
    };
  }
}
