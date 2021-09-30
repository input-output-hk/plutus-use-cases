// import { toast } from 'react-toastify';
import * as fromApi from '../api/games';
import {
  FETCH_GAMES_START,
  FETCH_GAMES_SUCCESS,
  FETCH_GAMES_FAILED,
  FETCH_GAME_START,
  FETCH_GAME_SUCCESS,
  FETCH_GAME_FAILED,
} from '../helpers/actionTypes';

export const fetchGamesStart = () => ({
  type: FETCH_GAMES_START,
});

export const fetchGamesSuccess = (games) => ({
  type: FETCH_GAMES_SUCCESS,
  games,
});

export const fetchGamesFailed = (error) => ({
  type: FETCH_GAMES_FAILED,
  error,
});

export const fetchGameStart = () => ({
  type: FETCH_GAME_START,
});

export const fetchGameSuccess = (game) => ({
  type: FETCH_GAME_SUCCESS,
  game,
});

export const fetchGameFailed = (error) => ({
  type: FETCH_GAME_FAILED,
  error,
});

export const fetchGames = () => async (dispatch) => {
  dispatch(fetchGamesStart());
  const games = await fromApi.fetchGames();
  if (games.error) {
    dispatch(fetchGamesFailed(games.error));
    // toast.error(games.error);
  } else {
    dispatch(fetchGamesSuccess(games));
  }
};

export const fetchGame = (id) => async (dispatch) => {
  dispatch(fetchGameStart());
  const game = await fromApi.fetchGame(id);
  if (game.error) {
    dispatch(fetchGameFailed(game.error));
    // toast.error(games.error);
  } else {
    dispatch(fetchGameSuccess(game));
  }
};