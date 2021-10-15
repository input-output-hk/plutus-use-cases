import { connect } from 'react-redux';
import { compose, lifecycle, withProps, withState } from 'recompose';
import moment from 'moment';
import { Button } from 'react-bootstrap';
import Bets from './Bets';
import MakeBet from './MakeBet';
import Loader from './Loader';
import { fetchGame } from '../actions/games';
import { fetchGameContract, fetchGameBets } from '../actions/bets';
import {
  getCurrentUser,
  getGame,
  getGameBets,
  getGameFetching,
  getGameBetsFetching,
} from '../reducers';
import { statusesMap } from '../helpers/constants';

import '../styles/Game.scss';

const Game = ({
  game,
  homeBets,
  awayBets,
  showModal,
  setShowModal,
  homeBorder,
  awayBorder,
  currentUser,
  gameFetching,
  betsFetching,
}) => (
  <>
    <div className='Game'>
      {game && game.fixture && (
        <>
          <section className='team'>
            <div className={`img-border ${homeBorder}`}>
              <img
                src={game.teams.home.logo}
                alt='team-logo'
                className='img left'
              />
            </div>
            <h2>{game.teams.home.name}</h2>
            <Bets bets={homeBets} currentUser={currentUser} />
          </section>
          <section className='game-details'>
            <span
              className={`status ${statusesMap[
                game.fixture.status.short
              ].toLowerCase()}`}
            >
              {statusesMap[game.fixture.status.short]}
            </span>
            <span className='date'>
              {moment(game.fixture.date).format('DD MMM HH:mm YYYY')}
            </span>
            <span className='referee'>Referee: {game.fixture.referee}</span>
            {game.fixture.status.short === 'NS' && (
              <Button variant='secondary' onClick={() => setShowModal(true)}>
                Make a bet
              </Button>
            )}
          </section>
          <section className='team'>
            <div className={`img-border ${awayBorder}`}>
              <img
                src={game.teams.away.logo}
                alt='team-logo'
                className='img right'
              />
            </div>
            <h2>{game.teams.away.name}</h2>
            <Bets
              bets={awayBets}
              currentUser={currentUser}
              betsFetching={betsFetching}
            />
          </section>
          <MakeBet
            showModal={showModal}
            setShowModal={setShowModal}
            game={game}
          />{' '}
        </>
      )}
      {gameFetching && (
        <Loader disableBackground={true} text={'Loading game...'} />
      )}
    </div>
    {betsFetching && (
      <div className={'bets-loader-container'}>
        <Loader
          disableBackground={false}
          text={'Loading bets...'}
          staticStyles={true}
        />
      </div>
    )}
  </>
);

const enhancer = compose(
  withState('showModal', 'setShowModal', false),
  connect(
    (state) => ({
      currentUser: getCurrentUser(state),
      game: getGame(state),
      bets: getGameBets(state),
      gameFetching: getGameFetching(state),
      betsFetching: getGameBetsFetching(state),
    }),
    (dispatch) => ({
      fetchGame: (id) => dispatch(fetchGame(id)),
      fetchGameContract: (id, gameId) =>
        dispatch(fetchGameContract(id, gameId)),
      fetchGameBets: (id) => dispatch(fetchGameBets(id)),
    })
  ),
  withProps(({ bets, game }) => ({
    homeBets: bets
      ? bets.filter((bet) => bet.betTeamId === game.teams.home.id)
      : [],
    awayBets: bets
      ? bets.filter((bet) => bet.betTeamId === game.teams.away.id)
      : [],
    homeBorder:
      game && game.fixture && game.fixture.status.short === 'FT'
        ? game.teams.home.winner
          ? 'winner'
          : 'defeated'
        : '',
    awayBorder:
      game && game.fixture && game.fixture.status.short === 'FT'
        ? game.teams.away.winner
          ? 'winner'
          : 'defeated'
        : '',
  })),
  lifecycle({
    componentDidMount() {
      const gameId = this.props.match.params.id;
      const { fetchGame, fetchGameContract, currentUser } = this.props;
      fetchGame(gameId);
      fetchGameContract(currentUser.walletId, gameId);
    },
    componentDidUpdate() {
      const { game, fetchGameBets, bets } = this.props;
      if (game && game.contractId && !bets) {
        fetchGameBets(game.contractId);
      }
    },
  })
);

export default enhancer(Game);