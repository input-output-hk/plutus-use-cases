// import { toast } from 'react-toastify';
import * as fromApi from '../api/bets';
import { getGameContract, parseBetsResponse } from '../helpers/utils';
import {
  FETCH_GAME_CONTRACT_START,
  FETCH_GAME_CONTRACT_SUCCESS,
  FETCH_GAME_CONTRACT_FAILED,
  FETCH_GAME_BETS_START,
  FETCH_GAME_BETS_SUCCESS,
  FETCH_GAME_BETS_FAILED,
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

export const fetchGameContract = (id, gameId) => async (dispatch) => {
  dispatch(fetchGameContractStart());
  const contracts = await fromApi.fetchContracts(id);
  if (contracts.error) {
    dispatch(fetchGameContractFailed(contracts.error));
    // toast.error(contracts.error);
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
    // toast.error(contracts.error);
  } else {
    const result = parseBetsResponse(bets)
    dispatch(fetchGameBetsSuccess(result));
  }
};