import { toast } from 'react-toastify';
import * as fromApi from '../api/bets';
import { getGameContract, parseBetsResponse } from '../helpers/utils';
import {
  FETCH_GAME_CONTRACT_START,
  FETCH_GAME_CONTRACT_SUCCESS,
  FETCH_GAME_CONTRACT_FAILED,
  FETCH_GAME_BETS_START,
  FETCH_GAME_BETS_SUCCESS,
  FETCH_GAME_BETS_FAILED,
  FETCH_MAKE_BET_START,
  FETCH_MAKE_BET_SUCCESS,
  FETCH_MAKE_BET_FAILED,
} from '../helpers/actionTypes';

export const fetchGameContractStart = () => ({
  type: FETCH_GAME_CONTRACT_START,
});

export const fetchGameContractSuccess = (contract) => ({
  type: FETCH_GAME_CONTRACT_SUCCESS,
  contract,
});

export const fetchGameContractFailed = (error) => ({
  type: FETCH_GAME_CONTRACT_FAILED,
  error,
});

export const fetchGameBetsStart = () => ({
  type: FETCH_GAME_BETS_START,
});

export const fetchGameBetsSuccess = (bets) => ({
  type: FETCH_GAME_BETS_SUCCESS,
  bets,
});

export const fetchGameBetsFailed = (error) => ({
  type: FETCH_GAME_BETS_FAILED,
  error,
});

export const fetchMakeBetStart = () => ({
  type: FETCH_MAKE_BET_START,
});

export const fetchMakeBetSuccess = (bet) => ({
  type: FETCH_MAKE_BET_SUCCESS,
  bet,
});

export const fetchMakeBetFailed = (error) => ({
  type: FETCH_MAKE_BET_FAILED,
  error,
});

export const fetchGameContract = (id, gameId) => async (dispatch) => {
  dispatch(fetchGameContractStart());
  const contracts = await fromApi.fetchContracts(id);
  if (contracts.error) {
    dispatch(fetchGameContractFailed(contracts.error));
    toast.error(contracts.error);
  } else {
    const contract = getGameContract(contracts, gameId);
    dispatch(fetchGameContractSuccess(contract));
  }
};

export const fetchGameBets = (id) => async (dispatch) => {
  dispatch(fetchGameBetsStart());
  const bets = await fromApi.fetchGameBets(id);
  if (bets.error) {
    dispatch(fetchGameBetsFailed(bets.error));
    toast.error(bets.error);
  } else {
    const result = parseBetsResponse(bets);
    dispatch(fetchGameBetsSuccess(result));
  }
};

export const makeBet = (team, amount, gameId, wallet) => async (dispatch) => {
  dispatch(fetchMakeBetStart());
  const bet = await fromApi.makeBet(team, amount, gameId);
  if (bet.error) {
    dispatch(fetchMakeBetFailed(bet.error));
    toast.error(bet.error);
  } else {
    dispatch(
      fetchMakeBetSuccess({
        betBettor: { getPubKeyHash: wallet.publicKey },
        betAmount: { getLovelace: amount },
        betTeamId: team,
      })
    );
    toast.success('Your bet has been accepted');
  }
};
