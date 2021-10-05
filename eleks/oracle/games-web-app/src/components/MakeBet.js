import { compose, withProps, withHandlers, withState } from 'recompose';
import { connect } from 'react-redux';
import { Modal, Button } from 'react-bootstrap';
import Select from 'react-select';
import { makeBet } from '../actions/bets';
import '../styles/MakeBet.scss';

const customTheme = (theme) => ({
  ...theme,
  colors: {
    ...theme.colors,
    primary50: '#cccccc',
    primary25: '#cccccc',
    primary: '#808080',
  },
});

const MakeBet = ({
  showModal,
  onClose,
  handleSubmit,
  teams,
  setTeam,
  amount,
  setAmount,
  error,
}) => (
  <Modal
    show={showModal}
    onHide={onClose}
    size='sm'
    aria-labelledby='contained-modal-title-vcenter'
    centered
    className='MakeBet'
  >
    <Modal.Header closeButton>
      <Modal.Title id='contained-modal-title-vcenter'>
        Make your bet
      </Modal.Title>
    </Modal.Header>
    <Modal.Body className='body'>
      <p>Please, enter the amount in Lovelace, that you want to bet.</p>
      <Select
        options={teams}
        onChange={(el) => setTeam(el.value)}
        placeholder={'Team'}
        theme={customTheme}
      />
      <input
        className={'input'}
        value={amount}
        onChange={(ev) => setAmount(ev.target.value)}
        placeholder={'Amount'}
      />
      <span className={`error-text ${error ? 'visible' : ''}`}>
        {error || 'error'}
      </span>
    </Modal.Body>
    <Modal.Footer className='footer'>
      <Button onClick={handleSubmit} variant='secondary' type='submit'>
        Make a bet
      </Button>
      <Button onClick={onClose} className='btn-danger'>
        Cancel
      </Button>
    </Modal.Footer>
  </Modal>
);

const enhancer = compose(
  withState('team', 'setTeam'),
  withState('amount', 'setAmount', ''),
  withState('error', 'setError', ''),
  withProps(({ game }) => {
    const home = game.teams.home;
    const away = game.teams.away;
    return {
      teams: [
        { value: home.id, label: home.name },
        { value: away.id, label: away.name },
      ],
    };
  }),
  connect(null, (dispatch) => ({
    makeBet: (team, amount, gameId) => dispatch(makeBet(team, amount, gameId)),
  })),
  withHandlers({
    onClose: ({ setError, setAmount, setShowModal }) => (ev) => {
      setError(null);
      setAmount('');
      setShowModal(false);
    },
  }),
  withHandlers({
    handleSubmit: ({ team, amount, makeBet, game, setError, onClose }) => (
      ev
    ) => {
      const reg = new RegExp('^[0-9]*$');
      const isNum = reg.test(amount);

      if (!team || !amount) {
        setError('All fields should be filled');
      } else if (!isNum || amount < 2000000) {
        setError(
          'Amount should be a number and can`t be less than 2000000 Lovelace'
        );
      } else {
        makeBet(team, amount, game.contractId);
        onClose();
      }
    },
  })
);

export default enhancer(MakeBet);
