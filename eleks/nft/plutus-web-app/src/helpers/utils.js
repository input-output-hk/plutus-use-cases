export const formatForAPI = (data) => ({
  cpTokenName: data.name,
  cpDescription: data.desc,
  cpAuthor: data.author,
  cpFile: data.image,
});

const dataMap = (token) => ({
  id: token.nftDtoTokenSymbol,
  name: token.nftDtoTokenName,
  author: token.nftDtoSeller,
  price: token.nftDtoSellPrice,
  image: token.nftDtoMetaFile,
  description: token.nftDtoMetaDescription,
});

export const formatArrayResponse = (data) => {
  const tokens = data.cicCurrentState.observableState.Right.contents;
  return tokens.map((token) => dataMap(token));
};
