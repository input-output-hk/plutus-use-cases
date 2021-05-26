export const formatForAPI = (data) => ({
  cpTokenName: data.name,
  cpDescription: data.desc,
  cpAuthor: data.author,
  cpFile: data.image,
});

export const formatSellData = (data) => ({
  spSellPrice: +data.price,
  spTokenSymbol: data.id,
});

export const formatBuyData = (data) => ({
  bpTokenSymbol: data.id,
});

const dataMap = (token) => ({
  id: token.nftDtoTokenSymbol,
  name: token.nftDtoTokenName,
  author: token.nftDtoMetaAuthor,
  seller: token.nftDtoSeller,
  price: token.nftDtoSellPrice,
  image: token.nftDtoMetaFile,
  description: token.nftDtoMetaDescription,
});

export const formatResponse = (data) => {
  const tokens = data.cicCurrentState.observableState.Right.contents;
  return tokens.map((token) => dataMap(token));
};

export const formatKeyResponse = (data) =>
  data.cicCurrentState.observableState.Right.contents;

export const wait = (ms) => new Promise((r) => setTimeout(() => r(), ms));
