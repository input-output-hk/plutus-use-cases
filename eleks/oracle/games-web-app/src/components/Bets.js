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
          {bet.betAmount.getLovelace / 1000000} ADA
        </span>
      </div>
    ))}
  </div>
);

export default Bets;
