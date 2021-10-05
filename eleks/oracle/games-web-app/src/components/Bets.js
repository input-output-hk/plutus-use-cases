import '../styles/Bets.scss';

const Bets = ({ bets }) => (
  <div className='Bets'>
    {bets.map((bet, i) => (
      <div className='bet' key={i}>
        <span className='hash'>{bet.betBettor.getPubKeyHash}</span>
        <span className='amount'>{bet.betAmount.getLovelace} Lovelace</span>
      </div>
    ))}
  </div>
);

export default Bets;
