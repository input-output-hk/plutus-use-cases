import { statusesMap } from './constants';

export const getGameContract = (contracts, gameId) => {
  const contract = contracts.find(
    (el) => el.cicDefinition.contents.mbpGame === +gameId
  );
  return {
    contractId: contract ? contract.cicContract.unContractInstanceId : null,
    minBet: contract
      ? lovelaceToAda(contract.cicDefinition.contents.mbpMinBet.getLovelace)
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

export const lovelaceToAda = (amount) => amount / 1000000;

export const adaToLovelace = (amount) => amount * 1000000;

export const isGameFinished = (game) =>
  game && game.fixture && statusesMap[game.fixture.status.short] === 'CLOSED';

export const isGameLive = (game) =>
  game && game.fixture && statusesMap[game.fixture.status.short] === 'LIVE';

export const isGameOpen = (game) =>
  game && game.fixture && statusesMap[game.fixture.status.short] === 'OPEN';
