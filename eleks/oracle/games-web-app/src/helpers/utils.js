import { statusesMap } from './constants';

export const getGameContract = (contracts, gameId) => {
  const contract = contracts.find(
    (el) => el.cicDefinition.contents[2].mbpGame === +gameId
  );
  return {
    contractId: contract ? contract.cicContract.unContractInstanceId : null,
    minBet: contract
      ? contract.cicDefinition.contents[2].mbpMinBet.getLovelace / 1000000
      : null,
  };
};

export const parseBetsResponse = (data) =>
  data.cicCurrentState.observableState.mutualBetState
    ? data.cicCurrentState.observableState.mutualBetState.contents
    : [];

export const sortByStatus = (data) =>
  data.sort((a, b) => {
    const order = Object.keys(statusesMap);
    if (
      order.indexOf(a.fixture.status.short) >
      order.indexOf(b.fixture.status.short)
    ) {
      return 1;
    }
    if (
      order.indexOf(a.fixture.status.short) <
      order.indexOf(b.fixture.status.short)
    ) {
      return -1;
    }
    return 0;
  });
