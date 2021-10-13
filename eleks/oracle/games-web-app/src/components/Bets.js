import { lovelaceToAda } from '../helpers/utils';
import '../styles/Bets.scss';

const Bets = ({ bets, currentUser }) => (
  <div className='Bets'>
    {bets.map((bet, i) => (
      <div
        className={`bet ${
          currentUser && bet.betBettor.getPubKeyHash === currentUser.publicKey
            ? 'myBet'
            : ''
        }`}
        key={i}
      >
        <span className='hash'>{bet.betBettor.getPubKeyHash}</span>
        <span className='amount'>
          {lovelaceToAda(bet.betAmount.getLovelace)} ADA
        </span>
      </div>
    ))}
  </div>
);

export default Bets;
