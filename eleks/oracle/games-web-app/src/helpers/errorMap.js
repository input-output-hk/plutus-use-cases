export const errorMap = (response, errorPlaceholder) => {
  const errors = {
    InsufficientFunds: 'Insufficient funds in the wallet',
  };
  const error = response.cicCurrentState.err.contents.contents.contents.tag;
  return errors[error] || errorPlaceholder;
};
