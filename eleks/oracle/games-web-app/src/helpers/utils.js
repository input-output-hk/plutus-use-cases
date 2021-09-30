export const getGameContract = (contracts, gameId) => {
  const contract = contracts.find(
    (el) => el.cicDefinition.contents[2].mbpGame === +gameId
  );
  return contract.cicContract.unContractInstanceId;
};

export const parseBetsResponse = (data) =>
  data.cicCurrentState.observableState.mutualBetState.contents;
